var step = 1;
var pst = 700;
var next = 710;
var wh;
function backrad(){
   if(next==1000){
     next=980;
     pst=990;
   }
   else if(next==690){
     next=710;
     pst=700;
   }
   else {
     if(next>pst){
       pst =next;
       next +=step;

     }
     else{
       pst =next;
       next -=step;

     }
   }
   if(next>800) wh = next-300;
  debugit(next);
  $('#video-background').css({
    background: 'radial-gradient(circle, white '+wh/10+'%, steelblue '+pst/10+'%)'});
}
setInterval(backrad,15);
