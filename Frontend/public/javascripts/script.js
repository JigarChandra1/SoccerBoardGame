window.onload = function() {
  // TODO: change to 5	
  const NUM_PLAYER_COINS = 12;
  //The initial setup
  // TODO: change board dimension to 9x9 in both frontend and backend, 11x11 is too broad
  var gameBoard = [ 
    [  0,  0,  0,  1,  0,  0,  0,  0 ],
    [  0,  0,  1,  0,  1,  0,  0,  0 ],
    [  0,  1,  0,  0,  0,  1,  0,  0 ],
    [  0,  0,  0,  0,  0,  0,  0,  0 ],
    [  0,  0,  0,  0,  0,  0,  0,  0 ],
    [  0,  2,  0,  0,  0,  2,  0,  0 ],
    [  0,  0,  2,  0,  2,  0,  0,  0 ],
    [  0,  0,  0,  2,  0,  0,  0,  0 ]
  ];

  function getGameBoard() {
	  // TODO: get game positions through REST call
    var initialBoard = [ 
    [  0,  1,  0,  1,  0,  1,  0,  1 ],
    [  1,  0,  1,  0,  1,  0,  1,  0 ],
    [  0,  1,  0,  1,  0,  1,  0,  1 ],
    [  0,  0,  0,  0,  0,  0,  0,  0 ],
    [  0,  0,  0,  0,  0,  0,  0,  0 ],
    [  2,  0,  2,  0,  2,  0,  2,  0 ],
    [  0,  2,  0,  2,  0,  2,  0,  2 ],
    [  2,  0,  2,  0,  2,  0,  2,  0 ]
  ];
	  
  }
	
  //arrays to store the instances
  var pieces = [];
  var tiles = []; 
  
  //distance formula
  var dist = function (x1, y1, x2, y2) {
    return Math.sqrt(Math.pow((x1-x2),2)+Math.pow((y1-y2),2));
  }
  //Piece object - there are 24 instances of them in a checkers game
  function Piece (element, position) {
    //linked DOM element
    this.element = element;
    //positions on gameBoard array in format row, column
    this.position = position; 
    //which player's piece is it
    this.player = '';
    //figure out player by piece id

    if(parseInt(this.element.attr("id")) < 5) {
      this.player = 1;
	  this.id = parseInt(this.element.attr("id"));
	}
    else {
      this.player = 2;
	  this.id = parseInt(this.element.attr("id")) - 5;
	}

    //makes object a king
    this.king = false;
    this.makeKing = function () {
      this.element.css("backgroundImage", "url('king"+this.player+".png')");
      this.king = true;
    }
    //moves the piece
    this.move = function (tile) {
	var currentPiece = this;
	var postData = makeMoveDataForPost(this.player, this.id, tile.position[0], tile.position[1]);
	var request = createCORSRequest("post", "http://localhost:9000/game/player/move", postData);
	if (request){
    	request.onload = function(){
			console.log('received response: ' + request.responseText);
      currentPiece.element.removeClass('selected');
      //remove the mark from Board.board and put it in the new spot
      Board.board[currentPiece.position[0]][currentPiece.position[1]] = 0;
      Board.board[tile.position[0]][tile.position[1]] = currentPiece.player;
      currentPiece.position = [tile.position[0], tile.position[1]];
      //change the css using board's dictionary
      currentPiece.element.css('top', Board.dictionary[currentPiece.position[0]]);
      currentPiece.element.css('left', Board.dictionary[currentPiece.position[1]]);
      Board.changePlayerTurn();
      return true;			
    	};
    	request.send();
	}		
	
	function makeMoveDataForPost(playerTeam, playerNumber, x, y) {
		var data = {};
		var moveObj = {};
		moveObj.playerTeam = playerTeam;
		moveObj.playerNumber = playerNumber;
		moveObj.x = x;
		moveObj.y = y;
		data.move = moveObj;
		return JSON.stringify(data);
	}
    };
    
    //tests if piece can jump anywhere
    this.canJumpAny = function () {
      if(this.canOpponentJump([this.position[0]+2, this.position[1]+2]) ||
         this.canOpponentJump([this.position[0]+2, this.position[1]-2]) ||
         this.canOpponentJump([this.position[0]-2, this.position[1]+2]) ||
         this.canOpponentJump([this.position[0]-2, this.position[1]-2])) {
        return true;
      } return false;
    };
    
    //tests if an opponent jump can be made to a specific place
    this.canOpponentJump = function(newPosition) {
      //find what the displacement is
      var dx = newPosition[1] - this.position[1];
      var dy = newPosition[0] - this.position[0];
      //make sure object doesn't go backwards if not a king
      if(this.player == 1 && this.king == false) {
        if(newPosition[0] < this.position[0]) return false;
      } else if (this.player == 2 && this.king == false) {
        if(newPosition[0] > this.position[0]) return false;
      }
      //must be in bounds
      if(newPosition[0] > 7 || newPosition[1] > 7 || newPosition[0] < 0 || newPosition[1] < 0) return false;
      //middle tile where the piece to be conquered sits
      var tileToCheckx = this.position[1] + dx/2;
      var tileToChecky = this.position[0] + dy/2;
      //if there is a piece there and there is no piece in the space after that
      if(!Board.isValidPlacetoMove(tileToChecky, tileToCheckx) && Board.isValidPlacetoMove(newPosition[0], newPosition[1])) {
        //find which object instance is sitting there
        for(pieceIndex in pieces) {
          if(pieces[pieceIndex].position[0] == tileToChecky && pieces[pieceIndex].position[1] == tileToCheckx) {
            if(this.player != pieces[pieceIndex].player) {
              //return the piece sitting there
              return pieces[pieceIndex];
            }
          }
        }
      }
      return false;
    };
    
    this.opponentJump = function (tile) {
      var pieceToRemove = this.canOpponentJump(tile.position);
      //if there is a piece to be removed, remove it
      if(pieceToRemove) {
        pieces[pieceIndex].remove();
        return true;
      }
      return false;
    };
    
    this.remove = function () {
      //remove it and delete it from the gameboard
      this.element.css("display", "none");
      if(this.player == 1) $('#player2').append("<div class='capturedPiece'></div>");
      if(this.player == 2) $('#player1').append("<div class='capturedPiece'></div>");
      Board.board[this.position[0]][this.position[1]] = 0;
      //reset position so it doesn't get picked up by the for loop in the canOpponentJump method
      this.position = [];
    }
  }
  
  function Tile (element, position) {
    //linked DOM element
    this.element = element;
    //position in gameboard
    this.position = position;
    //if tile is in range from the piece
    this.inRange = function(piece) {
      if(dist(this.position[0], this.position[1], piece.position[0], piece.position[1]) == Math.sqrt(2)) {
        //regular move
        return 'regular';
      } else if(dist(this.position[0], this.position[1], piece.position[0], piece.position[1]) == 2*Math.sqrt(2)) {
        //jump move
        return 'jump';
      }
    };
  }
  
  //Board object - controls logistics of game
  var Board = {
    board: gameBoard,
    playerTurn: 1,
    tilesElement: $('div.tiles'),
    //dictionary to convert position in Board.board to the viewport units
    dictionary: ["0vmin", "10vmin", "20vmin", "30vmin", "40vmin", "50vmin", "60vmin", "70vmin", "80vmin", "90vmin"],
    //initialize the 8x8 board
    initalize: function () {
      var countPieces = 0;
      var countTiles = 0;
      for (row in this.board) { //row is the index
        for (column in this.board[row]) { //column is the index
          //whole set of if statements control where the tiles and pieces should be placed on the board
          if(row%2 == 1) {
            if(column%2 == 0) {
              this.tilesElement.append("<div class='tile' id='tile"+countTiles+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
              tiles[countTiles] = new Tile($("#tile"+countTiles), [parseInt(row), parseInt(column)]);
              countTiles += 1;
            } else {
			  this.tilesElement.append("<div class='whitetile' id='tile"+countTiles+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
              tiles[countTiles] = new Tile($("#whitetile"+countTiles), [parseInt(row), parseInt(column)]);
              countTiles += 1;				
			}
          } else {
            if(column%2 == 1) {
              this.tilesElement.append("<div class='tile' id='tile"+countTiles+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
              tiles[countTiles] = new Tile($("#tile"+countTiles), [parseInt(row), parseInt(column)]);
              countTiles += 1;
            } else {
			  this.tilesElement.append("<div class='whitetile' id='tile"+countTiles+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
              tiles[countTiles] = new Tile($("#whitetile"+countTiles), [parseInt(row), parseInt(column)]);
              countTiles += 1;				
			}
          }
          if(this.board[row][column] == 1) {
            $('.player1pieces').append("<div class='piece' id='"+countPieces+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
            pieces[countPieces] = new Piece($("#"+countPieces), [parseInt(row), parseInt(column)]);
            countPieces += 1;
          } else if(this.board[row][column] == 2) {
            $('.player2pieces').append("<div class='piece' id='"+countPieces+"' style='top:"+this.dictionary[row]+";left:"+this.dictionary[column]+";'></div>");
            pieces[countPieces] = new Piece($("#"+countPieces), [parseInt(row), parseInt(column)]);
            countPieces += 1;
          }
        }
      }
    },
    //check if the location has an object
    isValidPlacetoMove: function (row, column) {
      console.log(row); console.log(column); console.log(this.board);
      if(this.board[row][column] == 0) {
        return true;
      } return false;
    },
    //change the active player - also changes div.turn's CSS
    changePlayerTurn: function () {
      if(this.playerTurn == 1) {
        this.playerTurn = 2;
        $('.turn').css("background", "linear-gradient(to right, transparent 50%, #BEEE62 50%)");
        return;
      }
      if(this.playerTurn == 2) {
        this.playerTurn = 1;
        $('.turn').css("background", "linear-gradient(to right, #BEEE62 50%, transparent 50%)");
      }
    },
    //reset the game
    clear: function () {
      location.reload(); 
    }
  }
  
  //initialize the board
  Board.initalize();
  
  /***
  Events
  ***/
  
  //select the piece on click if it is the player's turn
  $('.piece').on("click", function () {
	// TODO: just a test call
	var request = createCORSRequest("post", "http://localhost:9000/game/positions");
	if (request){
    	request.onload = function(){
			console.log('received response: ' + request.responseText);
    	};
    	request.send();
	}	  	  
    var selected;
    var isPlayersTurn = ($(this).parent().attr("class").split(' ')[0] == "player"+Board.playerTurn+"pieces");
    if(isPlayersTurn) {
      if($(this).hasClass('selected')) selected = true;
      $('.piece').each(function(index) {$('.piece').eq(index).removeClass('selected')});
      if(!selected) {
        $(this).addClass('selected');
      }
    }
  });
  
  //reset game when clear button is pressed
  $('#cleargame').on("click", function () {
    Board.clear();
  });
  
  //move piece when tile is clicked
  $('.tile').on("click", function () {
	processTileClick(this);
  });

  $('.whitetile').on("click", function () {
	processTileClick(this);
  });	
	
	function processTileClick(tileClicked) {
	    //make sure a piece is selected
    if($('.selected').length != 0) {
      //find the tile object being clicked
      var tileID = $(tileClicked).attr("id").replace(/tile/, '');
      var tile = tiles[tileID];
      //find the piece being selected
      var piece = pieces[$('.selected').attr("id")];
      //check if the tile is in range from the object
      var inRange = tile.inRange(piece);
// TODO: check if move is acceptable in terms of distance and if it is a tackle move		
			var info = getPieceInfoOnTile(tile.position[0], tile.position[1], piece.player);
            piece.move(tile);
    }
		
	  function getPieceInfoOnTile(tileXposition, tileYposition, currPlayer) {
		  var pieceInfo = {};
		  var obj = Board.board[tileXposition][tileYposition];
		  for(var i=0; i < pieces.length; i++) {
			  var currPiece = pieces[i];
			  if(currPiece.position[0] === tileXposition && currPiece.position[1] === tileYposition) {
				  if(currPiece.player === currPlayer) {
					  pieceInfo.hasOwnPlayersPiece=true;
				  } else {
					pieceInfo.hasOpponentPiece=true;  
				  }
				  pieceInfo.OpponentPieceId=currPiece.id;
			  }
		  }
		  return pieceInfo;
	  }	
	}

	function createCORSRequest(method, url, data){
    var xhr = new XMLHttpRequest();
    if ("withCredentials" in xhr){
        xhr.open(method, url, true);
		if(data) {
	       xhr.send(data);
		}		
    } else if (typeof XDomainRequest != "undefined"){
        xhr = new XDomainRequest();
        xhr.open(method, url);
		if(data) {
	       xhr.send(data);
		}
		
    } else {
        xhr = null;
    }
    return xhr;
	}
}