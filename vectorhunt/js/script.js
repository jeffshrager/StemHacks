var canvas, ctx, cwidth, cheight, origin;

var xz, yz; // Global screen centers

// The pt object autoregisters to the center of the screen

var pinc=19; // Pixels per unit on the real and imaginary scales

function pt(x,y){
    this.lx=x; // Logical positions are the ones given to us.
    this.ly=y;
    this.rx=xz+(x*pinc); // Real positions in screen coords.
    this.ry=yz+((-y)*pinc);
    return this;
}

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

function dv(from,to,color) { // Draw a vector, including arrow heads
    dl(from,to,color);
/*
    var x11 = (x1-(cwidth/2))/inc; // X11 and Y11 should always be = 0
    var y11 = -(y1-(cheight/2))/inc; // Y needs to get flipped over bcs 0 is at the top
    var x22 = (x2-(cwidth/2))/inc;
    var y22 = -(y2-(cheight/2))/inc;
    var slope=((y22-y11)/(x22-x11)); // Assume from center.
    var theta=Math.atan(slope)*57.3;
    ctx.fillStyle="White";
    ctx.fillText("("+x22.toFixed(2).toString()+","+y22.toFixed(2).toString()+")="+slope.toFixed(2).toString() + "->" + theta.toFixed(2).toString(),x2,y2);
*/
}

function dl2(fx,fy,tx,ty){
    ctx.moveTo(fx,fy);
    ctx.lineTo(tx,ty);
    ctx.stroke();   
}

function dl(from, to, color) { // draw a line (used all over the place)
    ctx.fillStyle = 'rgba(255, 110, 110, 0.5)';
    ctx.beginPath();
    ctx.lineWidth=3;
    ctx.strokeStyle = color;
    ctx.lineStyle="#123456";
    dl2(from.rx,from.ry,to.rx,to.ry)
    ctx.fillStyle="White";
    ctx.fillText("( "+to.lx+" , "+to.ly+" )",to.rx,to.ry);
}

function vadd(v1,v2){
    return new pt(v1.lx+v2.lx,v1.ly+v2.ly);
}

function vtimes(v1,v2){
    // (a+bi)(c+di) -> (ac-bd,bc+ad)
    var a = v1.lx;
    var b = v1.ly;
    var c = v2.lx;
    var d = v2.ly;
    return new pt(((a*c)-(b*d)),((b*c)+(a*d)));
}

function drawScene() { // main drawScene function
    clear(); 
    // axes
    var p=14;
    ctx.lineWidth=1;
    ctx.fillStyle="#FFFFFF";
    ctx.lineStyle="#000000";
    ctx.font="14px sans-serif";
    for (var i=32; i>-1; i--) { // display Y ruler.
	ctx.fillText((i-16).toString(), (cwidth/2)+5, p);
	p=p+18;
    }
    ctx.fillStyle="Yellow";
    ctx.fillText("imaginary axis", (cwidth/2)+40, 20);
    ctx.fillStyle="#FFFFFF";
    var p=15;
    for (var i=0; i<41; i++) { // display X ruler.
	ctx.fillText((i-20).toString(), p, (cheight/2)+15);
	p=p+19;
    }
    ctx.fillStyle="Yellow";
    ctx.fillText("real axis", cwidth-60,(cheight/2)+25);
    // Axes:
    var origin = new pt(0,0) 
    var x0 = new pt(-20,0);
    var x1 = new pt(+20,0);
    var y0 = new pt(0,-16);
    var y1 = new pt(0,+16);

    dl(x0,x1,'white');
    dl(y0,y1,'white');


    // Test vectors
    // First vector:

    var v1 = new pt(randn(20)-randn(10), randn(16)-randn(8));
    // A vector to get added or multipled to #1, above:
    var v2 = new pt(randn(20)-randn(10), randn(16)-randn(8));
    // Draw them both from the center:
    dv(origin,v1,'red');
    dv(origin,v2,'yellow');
    var v1plusv2 = vadd(v1,v2); // Compute sum
    dv(v1,v1plusv2,'yellow'); // Draw it from the tail of v1
    dv(origin,v1plusv2,'orange'); // And from the center
    var v1timesv2 = vtimes(v1,v2); // Compute the complex product by FOIL
    dv(origin,v1timesv2,'blue'); // And from the center
}

// -------------------------------------------------------------

// initialization

$(function(){
    canvas = document.getElementById('scene');
    ctx = canvas.getContext('2d');
    cwidth = canvas.width;
    cheight = canvas.height;
    // Global screen centers    
    xz = cwidth/2;
    yz = cheight/2;
    setInterval(drawScene, 3000); // loop drawScene
});