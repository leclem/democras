$(window).resize(function(){if(window.innerWidth > 830){backcanvas();$('#canvas').show();}else{$('#canvas').hide();}});
if(window.innerWidth >830){backcanvas();}
function backcanvas(){
var width = window.innerWidth;
var height = window.innerHeight;
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext('2d');
canvas.width = width;
canvas.height = height;
//ctx.translate (canvas.width/2,canvas.height/2);
var j =100;
var i = 200;
var raf;
//var x = j; // x coordinate
        var y = i; // y coordinate
        var radius = 20; // Arc radius
        var startAngle = 0; // Starting point on circle
        var endAngle = Math.PI+(Math.PI*j)/2; // End point on circle
        // clockwise or anticlockwise
        function themess(pmessage){
          if(pmessage == '?'){
            ctx.font = "40px octicons";
            var hasard = Math.random();
            if(hasard < 0.5){
              return "0xf077";
            }
            else {
              return "0xf0d8";
            }
          }
          else{
            ctx.font = "35px octicons";
            var hasard = Math.random();
            if(hasard < 0.25){
              return "0xf077";
            }
            else if(hasard < 0.5){
              return "0xf060";
            }
            else if(hasard < 0.75){
              return "0xf04f";
            }
            else {
              return "0xf0d8";
            }
          }
        }
var nh = (Math.random()*height - 10)+50;
var nh2 = (Math.random()*(height - 200))+100;// faire attention bulle en bas et en haut
var nh1 = (Math.random()*(height - 200))+100;
var bubble = function(px, py, pdy, prad, pmessage){
  this.age = new Date();
  this.x = px;
  this.y = py;
  this.dy = pdy;
  this.vx = 5;
  this.vy = 2;
  this.vr = 4;
  this.ar = 1.5;
  this.bubco = 0;
  this.radius = prad;
  this.tmessage = pmessage;
  this.message = themess(pmessage);
  this.completed = false;
  this.color = '#2c3e50';
  this.hovercolor = '#18bc9c';
  this.truecolor = '#2c3e50';
  this.draw = function() {
    ctx.fillStyle = this.color;
    ctx.beginPath();
    ctx.arc(this.x, this.y, this.radius, 0, Math.PI*2, true);
    ctx.closePath();
    ctx.lineWidth = 3;

    ctx.stroke();
    if(this.radius >= 15){
    //ctx.font = 'bold 16px helvetica';

      if(this.tmessage == '?'){
        ctx.font = "40px octicons";
        if(this.message=="0xf0d8"){
          ctx.fillText(String.fromCharCode(this.message), this.x - 17, this.y+10);
        }
        else {
          ctx.fillText(String.fromCharCode(this.message), this.x - 20, this.y+10);
        }

      }
      else{
        ctx.font = "35px octicons";
        if (this.message=="0xf060") {
          ctx.fillText(String.fromCharCode(this.message), this.x - 13, this.y+10);
        }
        else if (this.message=="0xf0d8") {
          ctx.fillText(String.fromCharCode(this.message), this.x - 15, this.y+10);
        }
        else {
          ctx.fillText(String.fromCharCode(this.message), this.x - 16, this.y+10);
        }
      }
    }
  };
  this.bappear = function(){
    if(this.y >= this.dy){
      this.y -= this.vy;
    }
    else{
      this.completed = true;
    }
  };
  this.sappear = function(){
    if(this.radius < 26)
      { this.vr = 4;
        this.radius += this.vr;
      }
    else if(this.radius > 26){
      this.vr = 3;
      this.radius -= this.vr;
    }
    else
      {
        this.completed = true;
      }
  };
};


function draw() {
  ctx.clearRect(0,0, canvas.width, canvas.height);
  //ball.draw(26);
  bub1.draw();
  bubp1.draw();
  bubp2.draw();
  bubp3.draw();
  bubp4.draw();
  bub2.draw();
  bub2p1.draw();
  bub2p2.draw();
  bub2p3.draw();
  bub2p4.draw();
  if(!bub1.completed){
  bub1.bappear();
}
else{
  if(!bubp1.completed){
    bubp1.sappear();
  }
  else{
    if(!bubp2.completed){
      bubp2.sappear();
    }
    else{
      if(!bubp3.completed){
        bubp3.sappear();
      }
      else{
        if(!bubp4.completed){
          bubp4.sappear();
        }
        else{
          if(!bub2.completed){
            bub2.bappear();
          }
          else{
            if(!bub2p1.completed){
              bub2p1.sappear();
            }
            else{
              if(!bub2p2.completed){
                bub2p2.sappear();
              }
              else{
                if(!bub2p3.completed){
                  bub2p3.sappear();
                }
                else{
                  if(!bub2p4.completed){
                    bub2p4.sappear();
                  }

                }
              }
            }
          }
        }
      }
    }
  }
}

  //verif age de la ball pour receer une autre
  window.requestAnimationFrame(draw);
}
//ball.draw();
var bub1 = new bubble(150, height, nh2, 35, "?");
bub1.draw();
var bubp1co = cpb(150, nh2);
var bubp1 = new bubble(bubp1co[0], bubp1co[1], nh2, 0, "!");
bubp1.draw();
var bubp2 = new bubble(bubp1co[2], bubp1co[3], nh2+75, 0, "!");
bubp2.draw();
var bubp3 = new bubble(bubp1co[4], bubp1co[5], nh2+75, 0, "!");
bubp3.draw();
var bubp4 = new bubble(bubp1co[6], bubp1co[7], nh2+75, 0, "!");
bubp4.draw();
var bub2 = new bubble(width-150, height+100, nh1, 35, "?");
bub2.draw();
var bub2co = cpb(width-150, nh1);
var bub2p1 = new bubble(bub2co[0], bub2co[1], nh2, 0, "!");
bub2p1.draw();
var bub2p2 = new bubble(bub2co[2], bub2co[3], nh2+75, 0, "!");
bub2p2.draw();
var bub2p3 = new bubble(bub2co[4], bub2co[5], nh2+75, 0, "!");
bub2p3.draw();
var bub2p4 = new bubble(bub2co[6], bub2co[7], nh2+75, 0, "!");
bub2p4.draw();
window.requestAnimationFrame(draw);

function cpb(pgx, pgy){
  var x1 = (Math.random()* 75);
  var y1 = Math.sqrt(Math.pow(75,2) - Math.pow(x1,2));
  var plusOrMinus1 = Math.random() < 0.5 ? -1 : 1;
  var plusOrMinus2 = Math.random() < 0.5 ? -1 : 1;
  x1 = plusOrMinus1*x1;
  y1 = plusOrMinus2*y1;
  var rx1 = pgx + x1;
  var ry1 = pgy + y1;
  var rx2 = pgx - x1;
  var ry2 = pgy - y1;
  var rx3 = pgx - y1;
  var ry3 = pgy + x1;
  var rx4 = pgx + y1;
  var ry4 = pgy - x1;
  return [rx1, ry1, rx2, ry2, rx3, ry3, rx4, ry4];
}
var canvasOffset = $("#canvas").offset();
var offsetX = canvasOffset.left;
var offsetY = canvasOffset.top;
function handleMouseMove(e,myCircle) {
    mouseX = parseInt(e.clientX - offsetX);
    mouseY = parseInt(e.clientY - offsetY);
    var dx = mouseX - myCircle.x;
    var dy = mouseY - myCircle.y;
    if (dx * dx + dy * dy < myCircle.radius*myCircle.radius) {
      myCircle.color = myCircle.hovercolor;

    } else {
      myCircle.color = myCircle.truecolor;
    }
}

$("body").mousemove(function (e) {
    handleMouseMove(e,bub1);
    handleMouseMove(e,bubp1);
    handleMouseMove(e,bubp2);
    handleMouseMove(e,bubp3);
    handleMouseMove(e,bubp4);
    handleMouseMove(e,bub2);
    handleMouseMove(e,bub2p1);
    handleMouseMove(e,bub2p2);
    handleMouseMove(e,bub2p3);
    handleMouseMove(e,bub2p4);
});
}
