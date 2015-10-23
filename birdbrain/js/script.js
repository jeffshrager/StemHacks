var canvas, ctx;
var birds = [];

// -------------------------------------------------------------

// objects :

function Bird(x,y,color){
    this.image = new Image();
    this.image.src = "images/"+color+".png";
    this.x = x;
    this.y = y;
}

// -------------------------------------------------------------

// Utils:

function randn(n) {return(Math.floor(Math.random()*n))};

// -------------------------------------------------------------

// draw functions :

function clear() { // clear canvas function
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
}

function drawBird(ctx, x, y, bno) { // draw a bird in a particular location
    ctx.drawImage(birds[bno].image, x, y, 30, 30);
}

function drawScene() { // main drawScene function
    clear(); 
    // birds
    for (var i=0; i<birds.length; i++) { // display all our birds
        drawBird(ctx, birds[i].x, birds[i].y, i);
    }
    // Table
    alert("foo");
    document.getElementById("mytable").style.left = randn(1000);
}

// -------------------------------------------------------------

// initialization

$(function(){
    canvas = document.getElementById('scene');
    ctx = canvas.getContext('2d');

    var width = canvas.width;
    var height = canvas.height;

    var birdcolors = ["red", "orange", "yellow", "green", "blue"]; 

    var ncolors = birdcolors.length;
    for (var i = 0; i < ncolors; i++) {
	var color = birdcolors[i];
        var x = 300+randn(width-300);
        var y = randn(height-300)+200;
        birds.push(new Bird(x,y,color));
    }

    var container = document.getElementById('container');
    var img = document.createElement("img");
    img.src = "images/red.png";
    img.width = 50;
    img.height = 50;
    container.appendChild(img);

    /*
    $('#scene').mouseup(function(e) { // on mouseup - cleaning selectedBird
        selectedBird = undefined;
    });
    */
    setInterval(drawScene, 30); // loop drawScene
});