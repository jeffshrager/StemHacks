var canvas, ctx;
var birds = [];
var rampxtop = 300;
var rampytop = 300;
var selectedBird;
var hoveredBird;
var launch = false;
var wormx = 0;
var wormy = 0;
var wormxslope = undefined;
var wormyslope = undefined;

var worm = new Image();
worm.src = "images/worm.png";

// -------------------------------------------------------------

// objects :

function Bird(x, y, bno){
    this.image = new Image();
    this.image.src = "images/b"+bno+".png";
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
    // axes
    var p=14;
    ctx.lineWidth=1;
    ctx.fillStyle="#FFFFFF";
    ctx.lineStyle="#000000";
    ctx.font="14px sans-serif";
    for (var i=40; i>-1; i--) { // display Y ruler.
	ctx.fillText(i.toString(), 5, p);
	p=p+13;
    }
    ctx.fillStyle="Yellow";
    ctx.fillText("Y axis", 30,14);
    ctx.fillStyle="#FFFFFF";
    var p=15;
    for (var i=0; i<41; i++) { // display X ruler.
	ctx.fillText(i.toString(), p, 580);
	p=p+19;
    }
    ctx.fillStyle="Yellow";
    ctx.fillText("X axis", 750,560);

    // launcher
    ctx.fillStyle = 'rgba(255, 110, 110, 0.5)';
    ctx.beginPath();
    ctx.lineWidth=5;
    ctx.lineStyle="#123456";
    ctx.moveTo(10, 570);
    ctx.lineTo(rampxtop,rampytop);
    ctx.stroke();   
    ctx.fillStyle="Yellow";
    ctx.fillText("Launch Vector", rampxtop-100,rampytop+20);

    // trace lines
    ctx.fillStyle = 'rgba(255, 110, 110, 0.5)';
    ctx.beginPath();
    ctx.lineWidth=1;
    ctx.strokeStyle = 'red';
    ctx.lineStyle="#123456";
    ctx.moveTo(0,rampytop);
    ctx.lineTo(rampxtop, rampytop);
    ctx.stroke();   
    ctx.moveTo(rampxtop,600);
    ctx.lineTo(rampxtop, rampytop);
    ctx.stroke();   

    // launch angle
    ctx.font="20px sans-serif";
    ctx.fillStyle="Yellow";
    ctx.strokeStyle = 'yellow';
    var launchangleinrads=Math.atan((ctx.canvas.height-rampytop)/rampxtop);
    ctx.fillText("Launch angle="+Math.floor(launchangleinrads*57.3)+" degrees", 60, 560)
    ctx.fillStyle = 'magenta';
    ctx.beginPath();
    ctx.lineWidth = 5;
    ctx.arc(0, 580, 50, 0, -launchangleinrads, true);
    ctx.stroke();
    ctx.closePath();

    // Sun (Launch button)
    ctx.fillStyle = 'yellow';
    ctx.beginPath();
    ctx.lineWidth = 1;
    ctx.strokeStyle = 'black';
    ctx.arc(740, 55, 50, 0, Math.PI*2, true);
    ctx.stroke();
    ctx.closePath();
    ctx.fill();
    ctx.fillStyle="#000000";
    ctx.lineStyle="#000000";
    ctx.font="14px sans-serif";
    ctx.fillText("Launch Worm", 695, 60);

    // Move the worm if it's been launched
    if (launch)
	{
	    ctx.drawImage(worm, wormx, wormy, 30, 30);
	    wormx=wormx+wormxslope;
	    wormy=wormy-wormyslope;
	    wormyslope=wormyslope-1; // Gravity!
	    if (wormy<10) launch=false;
	}

    // birds
    for (var i=0; i<birds.length; i++) { // display all our birds
        drawBird(ctx, birds[i].x, birds[i].y, i);
    }

    // If the worm hit a bird, remove that bird!
    if (launch)
	{
	    for (var i=0; i<birds.length; i++) 
		if ((Math.abs(birds[i].x-wormx)<15)&&(Math.abs(birds[i].y-wormy)<15))
		    {
		    switch (randn(3))
			{
			case 0: {alert("Great shot! That's a very happy birdie!"); break;}
			case 1: {alert("Nice job! You fed the bird a worm!"); break;}
			case 2: {alert("Yum yum! The bird's munching on that one!!"); break;}
			}
		    birds.splice(i, 1);
		    launch=false;
		    break;
		    }
	}

    // Game over?

    if (0==birds.length) 
	{
	    ctx.fillStyle="Magenta";
	    ctx.lineStyle="Black";
	    ctx.font="80px sans-serif";
	    ctx.fillText("You won!!!", 230, 200);
	}
}

// -------------------------------------------------------------

// initialization

$(function(){
    canvas = document.getElementById('scene');
    ctx = canvas.getContext('2d');

    var width = canvas.width;
    var height = canvas.height;

    var birdCount = 7; // !!! Must match filenames as "b1.png", "b2.png", etc.
    for (var i=1; i<=birdCount; i++) {
        var x = 300+randn(width-300);
        var y = randn(height-300)+200;
        birds.push(new Bird(x,y,i));
    }

    // binding mousedown event for aiming. Only do this when the click is w/i the axes, and then
    // only change the relevant axis!
    $('#scene').mousedown(function(e) {
        var canvasPosition = $(this).offset();
        var mouseX = e.layerX || 0;
        var mouseY = e.layerY || 0;
	// Check for launch button hit
	if ((Math.abs(mouseX-740)<40)&&(Math.abs(mouseY-55)<40)) 
	    {
		launch=true;
		wormx = 10;
		wormy = 570;
		wormxslope = (Math.floor(Math.abs(rampxtop-wormx)/10));
		wormyslope = (Math.floor(Math.abs(rampytop-wormy)/10));

	    }
	// Otherwise, move launcher if near the axes
	else if (mouseX < 40) rampytop = mouseY;
	else if (mouseY > 560) rampxtop = mouseX;
	else {alert ("To move the ramp, click on a number at the bottom or side.")};
        }
    );
    /*
    $('#scene').mousemove(function(e) { // binding mousemove event for selected bird
            var mouseX = e.layerX || 0;
            var mouseY = e.layerY || 0;
        if (selectedBird != undefined) {
            var canvasPosition = $(this).offset();
            var radius = birds[selectedBird].radius;
            birds[selectedBird] = new Bird(mouseX, mouseY,radius); // changing position of selected bird
        }
    }
    );
    */

    $('#scene').mouseup(function(e) { // on mouseup - cleaning selectedBird
        selectedBird = undefined;
    });

    setInterval(drawScene, 30); // loop drawScene
});