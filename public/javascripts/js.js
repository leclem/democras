$(document).ready(function () {
		//$('#wrongg').hide();
		//$('#wrong1').hide();
/**
$("button#LogInButton").click(function(){
	if ($('#email').val()==="") {
      // invalid
      $('#wrongg').show();
      return false;
    }
    if ($('#password').val()==="") {
      // invalid
      $('#wrongg2').show();
      return false;
    }
    else {
				$.ajax({
					type: "POST",
					url: "process.php", //
					data: $('form#logInForm').serialize(),
					success: function(msg){
						//$("#signupModal").modal('hide')
						$(".presentation").html(msg)
						$("#logInForm").hide();
					},
					error: function(){
						alert("failure");
					}
				});
				}
			});

**/
});
$('#messageelement').popover({
            html:true,
})
//$('#messageelement').popover.css("position","fixed");

$('#element').popover('hide')
$('#messageelement').popover('hide')
$('#notificationelement').popover('hide')
//$('#settingselement').popover('hide')

$("span#messageelement").click(function(){
$('#element').popover('hide')
$('#notificationelement').popover('hide')
//$('#settingselement').popover('hide')

});
$("span#notificationelement").click(function(){
$('#messageelement').popover('hide')
$('#element').popover('hide')
//$('#settingselement').popover('hide')

});
$("span#settingselement").click(function(){
$('#messageelement').popover('hide')
$('#notificationelement').popover('hide')
//$('#element').popover('hide')

});
$("span#element").click(function(){
$('#messageelement').popover('hide')
$('#notificationelement').popover('hide')
//$('#settingselement').popover('hide')

});


$(function(){
//$(".searchnavbar").hide()
  var header = $(".navbar-fixed-top"),
  	  inputh = $("input#searchnavbar"),
  	  inputhh = $("li#lisearch"),
      yOffset = 0,
      triggerPoint = 50;
  $(window).scroll(function(){
    yOffset = $(window).scrollTop();
    if(yOffset >= triggerPoint){
			
      if(window.innerWidth > 767){
        header.addClass("navbar-sm");
        $('.xspicfl').hide();
		  $('.xspseudofl').hide();
		  $('.xsseparatorfl').hide();
        $('#lsco').hide();
      }

      //inputh.removeClass("searchvbar"),
      //inputh.removeClass("lisearch"),
      //inputh.addClass("searchnavbar");
      //inputhh.style.width = "35px";
    }
		else{
    header.removeClass("navbar-sm");
		$('.xspicfl').fadeIn();
		$('.xspseudofl').fadeIn();
			$('.xsseparatorfl').fadeIn();
    $('#lsco').fadeIn();
      //header.removeClass("navbar-sm"),
      //header.removeClass("lisearch"),
      //inputh.addClass("searchvbar");
      //inputhh.style.width = "19px";

    }

  });
});
!function(){"use strict";function t(t,n){if("function"!=typeof n&&null!==n)throw new TypeError("Super expression must either be null or a function, not "+typeof n);t.prototype=Object.create(n&&n.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}),n&&(t.__proto__=n)}function n(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function e(t,n,e,o){for(var r=0,i=t.length,a=[],s=void 0;i>r&&(s=n.next(t[r]));)n=s,r++;if(r>=i)return[];for(;i-1>r;)s=new h(o),a.push(s),n.on(t[r],s),n=s,r++;return s=new h(e),a.push(s),n.on(t[i-1],s),a}function o(t,n){if("function"!=typeof n&&null!==n)throw new TypeError("Super expression must either be null or a function, not "+typeof n);t.prototype=Object.create(n&&n.prototype,{constructor:{value:t,enumerable:!1,writable:!0,configurable:!0}}),n&&(t.__proto__=n)}function r(t,n){if(!(t instanceof n))throw new TypeError("Cannot call a class as a function")}function i(t){return t instanceof d||t instanceof _}function a(t){return t}function s(t,n){return"url"===n?"_blank":null}function u(t){return t=t||{},{attributes:t.linkAttributes||null,defaultProtocol:t.defaultProtocol||"http",events:t.events||null,format:t.format||a,formatHref:t.formatHref||a,newLine:t.newLine||!1,nl2br:!!t.newLine||t.nl2br||!1,tagName:t.tagName||"a",target:t.target||s,linkClass:t.linkClass||"linkified"}}function l(t){for(var n=arguments.length,e=Array(n>1?n-1:0),o=1;n>o;o++)e[o-1]=arguments[o];return"function"==typeof t?t.apply(void 0,e):t}function c(t){if(t&&t.__esModule)return t;var n={};if(null!=t)for(var e in t)Object.prototype.hasOwnProperty.call(t,e)&&(n[e]=t[e]);return n["default"]=t,n}var p={__esModule:!0},f=function(){function t(e){n(this,t),this.j=[],this.T=e||null}return t.prototype.on=function(t,n){if(t instanceof Array)for(var e=0;e<t.length;e++)this.j.push([t[e],n]);else this.j.push([t,n])},t.prototype.next=function(t){for(var n=0;n<this.j.length;n++){var e=this.j[n],o=e[1];if(this.test(t,e[0]))return o}return!1},t.prototype.accepts=function(){return!!this.T},t.prototype.test=function(t,n){return t===n},t.prototype.emit=function(){return this.T},t}(),h=function(e){function o(){n(this,o),null!=e&&e.apply(this,arguments)}return t(o,e),o.prototype.test=function(t,n){return t===n||n instanceof RegExp&&n.test(t)},o}(f),g=function(e){function o(){n(this,o),null!=e&&e.apply(this,arguments)}return t(o,e),o.prototype.test=function(t,n){return t instanceof n},o}(f);p.CharacterState=h,p.TokenState=g,p.stateify=e;var m={__esModule:!0},y=function(){function t(n){r(this,t),this.v=n}return t.prototype.toString=function(){return this.v+""},t}(),d=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),b=function(t){function n(){r(this,n),t.call(this,"@")}return o(n,t),n}(y),v=function(t){function n(){r(this,n),t.call(this,":")}return o(n,t),n}(y),k=function(t){function n(){r(this,n),t.call(this,".")}return o(n,t),n}(y),w=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),x=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),O=function(t){function n(){r(this,n),t.call(this,"\n")}return o(n,t),n}(y),L=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),S=function(t){function n(){r(this,n),t.call(this,"+")}return o(n,t),n}(y),T=function(t){function n(){r(this,n),t.call(this,"#")}return o(n,t),n}(y),j=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),z=function(t){function n(){r(this,n),t.call(this,"?")}return o(n,t),n}(y),N=function(t){function n(){r(this,n),t.call(this,"/")}return o(n,t),n}(y),A=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),_=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),C=function(t){function n(){r(this,n),null!=t&&t.apply(this,arguments)}return o(n,t),n}(y),U={Base:y,DOMAIN:d,AT:b,COLON:v,DOT:k,PUNCTUATION:w,LOCALHOST:x,NL:O,NUM:L,PLUS:S,POUND:T,QUERY:z,PROTOCOL:j,SLASH:N,SYM:A,TLD:_,WS:C},M=function(){function t(n){r(this,t),this.v=n,this.type="token",this.isLink=!1}return t.prototype.toString=function(){for(var t=[],n=0;n<this.v.length;n++)t.push(this.v[n].toString());return t.join("")},t.prototype.toHref=function(){return this.toString()},t.prototype.toObject=function(t){return{type:this.type,value:this.toString(),href:this.toHref(void 0===t?"http":t)}},t}(),P=function(t){function n(e){r(this,n),t.call(this,e),this.type="email",this.isLink=!0}return o(n,t),n.prototype.toHref=function(){return"mailto:"+this.toString()},n}(M),E=function(t){function n(e){r(this,n),t.call(this,e),this.type="text"}return o(n,t),n}(M),D=function(t){function n(e){r(this,n),t.call(this,e),this.type="nl"}return o(n,t),n}(M),H=function(t){function n(e){r(this,n),t.call(this,e),this.type="url",this.isLink=!0}return o(n,t),n.prototype.toHref=function(t){t=void 0===t?"http":t;for(var n=!1,e=!1,o=this.v,r=[],a=0;o[a]instanceof j;)n=!0,r.push(o[a].toString().toLowerCase()),a++;for(;o[a]instanceof N;)e=!0,r.push(o[a].toString()),a++;for(;i(o[a]);)r.push(o[a].toString().toLowerCase()),a++;for(;a<o.length;a++)r.push(o[a].toString());return r=r.join(""),n||e||(r=t+"://"+r),r},n.prototype.hasProtocol=function(){return this.v[0]instanceof j},n}(M),q={Base:M,EMAIL:P,NL:D,TEXT:E,URL:H};m.text=U,m.multi=q;var R={__esModule:!0},I=m,Y=p,Q=function(t){return new Y.TokenState(t)},B=I.text.DOMAIN,K=I.text.AT,W=I.text.COLON,X=I.text.DOT,F=I.text.PUNCTUATION,G=I.text.LOCALHOST,J=I.text.NL,V=I.text.NUM,Z=I.text.PLUS,$=I.text.POUND,tt=I.text.PROTOCOL,nt=I.text.QUERY,et=I.text.SLASH,ot=I.text.SYM,rt=I.text.TLD,it=I.multi.EMAIL,at=I.multi.NL,st=I.multi.TEXT,ut=I.multi.URL,lt=Q(),ct=Q(),pt=Q(),ft=Q(),ht=Q(),gt=Q(),mt=Q(ut),yt=Q(),dt=Q(ut),bt=Q(),vt=Q(),kt=Q(ut),wt=Q(),xt=Q(ut),Ot=Q(ut),Lt=Q(),St=Q(),Tt=Q(),jt=Q(it),zt=Q(),Nt=Q(it),At=Q(),_t=Q(),Ct=Q(),Ut=Q(at);lt.on(J,Ut),lt.on(tt,ct),lt.on(et,pt),ct.on(et,pt),pt.on(et,ft),lt.on(rt,ht),lt.on(B,ht),lt.on(G,mt),lt.on(V,ht),ft.on(rt,bt),ft.on(B,bt),ft.on(V,bt),ft.on(G,kt),ht.on(X,gt),bt.on(X,vt),St.on(X,Tt),gt.on(rt,mt),gt.on(B,ht),gt.on(V,ht),gt.on(G,ht),vt.on(rt,kt),vt.on(B,bt),vt.on(V,bt),vt.on(G,bt),Tt.on(rt,jt),Tt.on(B,St),Tt.on(V,St),Tt.on(G,St),mt.on(X,gt),kt.on(X,vt),jt.on(X,Tt),mt.on(W,yt),mt.on(et,Ot),yt.on(V,dt),dt.on(et,Ot),kt.on(W,wt),kt.on(et,Ot),wt.on(V,xt),xt.on(et,Ot),jt.on(W,zt),zt.on(V,Nt);var Mt=[B,K,G,V,Z,$,tt,et,rt,ot],Pt=[W,X,nt,F];Ot.on(Mt,Ot),Lt.on(Mt,Ot),Ot.on(Pt,Lt),Lt.on(Pt,Lt);var Et=[B,V,Z,$,nt,ot,rt];ht.on(Et,At),ht.on(K,_t),gt.on(Et,At),mt.on(Et,At),mt.on(K,_t),At.on(Et,At),At.on(K,_t),At.on(X,Ct),Ct.on(Et,At),_t.on(rt,St),_t.on(B,St),_t.on(G,jt);var Dt=function(t){for(var n=t.length,e=0,o=[],r=[];n>e;){for(var i=lt,a=null,s=null,u=0,l=null,c=-1;n>e&&!(a=i.next(t[e]));)r.push(t[e++]);for(;n>e&&(s=a||i.next(t[e]));)a=null,i=s,i.accepts()?(c=0,l=i):c>=0&&c++,e++,u++;if(0>c)for(u=e-u;e>u;u++)r.push(t[u]);else 0<r.length&&(o.push(new st(r)),r=[]),e-=c,u-=c,i=l.emit(),o.push(new i(t.slice(e-u,e)))}return 0<r.length&&o.push(new st(r)),o},Ht=I.multi,qt=lt;R.State=Y.TokenState,R.TOKENS=Ht,R.run=Dt,R.start=qt;var Rt={__esModule:!0},It=m,Yt=p,Qt="abogado ac academy accountants active actor ad adult ae aero af ag agency ai airforce al allfinanz alsace am an android ao aq aquarelle ar archi army arpa as asia associates at attorney au auction audio autos aw ax axa az ba band bar bargains bayern bb bd be beer berlin best bf bg bh bi bid bike bio biz bj black blackfriday bloomberg blue bm bmw bn bnpparibas bo boo boutique br brussels bs bt budapest build builders business buzz bv bw by bz bzh ca cab cal camera camp cancerresearch capetown capital caravan cards care career careers casa cash cat catering cc cd center ceo cern cf cg ch channel cheap christmas chrome church ci citic city ck cl claims cleaning click clinic clothing club cm cn co coach codes coffee college cologne com community company computer condos construction consulting contractors cooking cool coop country cr credit creditcard cricket crs cruises cu cuisinella cv cw cx cy cymru cz dad dance dating day de deals degree delivery democrat dental dentist desi diamonds diet digital direct directory discount dj dk dm dnp do domains durban dvag dz eat ec edu education ee eg email emerck energy engineer engineering enterprises equipment er es esq estate et eu eurovision eus events everbank exchange expert exposed fail farm fashion feedback fi finance financial firmdale fish fishing fitness fj fk flights florist flsmidth fly fm fo foo forsale foundation fr frl frogans fund furniture futbol ga gal gallery gb gbiz gd ge gent gf gg gh gi gift gifts gives gl glass gle global globo gm gmail gmo gmx gn google gop gov gp gq gr graphics gratis green gripe gs gt gu guide guitars guru gw gy hamburg haus healthcare help here hiphop hiv hk hm hn holdings holiday homes horse host hosting house how hr ht hu ibm id ie il im immo immobilien in industries info ing ink institute insure int international investments io iq ir irish is it je jetzt jm jo jobs joburg jp juegos kaufen ke kg kh ki kim kitchen kiwi km kn koeln kp kr krd kred kw ky kz la lacaixa land latrobe lawyer lb lc lds lease legal lgbt li life lighting limited limo link lk loans london lotto lr ls lt ltda lu luxe luxury lv ly ma madrid maison management mango market marketing mc md me media meet melbourne meme memorial menu mg mh miami mil mini mk ml mm mn mo mobi moda moe monash money mormon mortgage moscow motorcycles mov mp mq mr ms mt mu museum mv mw mx my mz na nagoya name navy nc ne net network neustar new nexus nf ng ngo nhk ni ninja nl no np nr nra nrw nu nyc nz okinawa om ong onl ooo org organic otsuka ovh pa paris partners parts party pe pf pg ph pharmacy photo photography photos physio pics pictures pink pizza pk pl place plumbing pm pn pohl poker porn post pr praxi press pro prod productions prof properties property ps pt pub pw py qa qpon quebec re realtor recipes red rehab reise reisen reit ren rentals repair report republican rest restaurant reviews rich rio rip ro rocks rodeo rs rsvp ru ruhr rw ryukyu sa saarland sarl sb sc sca scb schmidt schule science scot sd se services sexy sg sh shiksha shoes si singles sj sk sl sm sn so social software sohu solar solutions soy space spiegel sr st su supplies supply support surf surgery suzuki sv sx sy sydney systems sz taipei tatar tattoo tax tc td technology tel tf tg th tienda tips tirol tj tk tl tm tn to today tokyo tools top town toys tp tr trade training travel trust tt tui tv tw tz ua ug uk university uno uol us uy uz va vacations vc ve vegas ventures versicherung vet vg vi viajes villas vision vlaanderen vn vodka vote voting voto voyage vu wales wang watch webcam website wed wedding wf whoswho wien wiki williamhill wme work works world ws wtc wtf xxx xyz yachts yandex ye yoga yokohama youtube yt za zip zm zone zw".split(" "),Bt=/[0-9]/,Kt=/[a-z0-9]/,Wt=":",Xt=[],Ft=function(t){return new Yt.CharacterState(t)},Gt=It.text.DOMAIN,Jt=It.text.LOCALHOST,Vt=It.text.NUM,Zt=It.text.PROTOCOL,$t=It.text.TLD,tn=It.text.WS,nn=Ft(),en=Ft(Vt),on=Ft(Gt),rn=Ft(),an=Ft(tn);nn.on("@",Ft(It.text.AT)),nn.on(".",Ft(It.text.DOT)),nn.on("+",Ft(It.text.PLUS)),nn.on("#",Ft(It.text.POUND)),nn.on("?",Ft(It.text.QUERY)),nn.on("/",Ft(It.text.SLASH)),nn.on(Wt,Ft(It.text.COLON)),nn.on(/[,;!]/,Ft(It.text.PUNCTUATION)),nn.on(/\n/,Ft(It.text.NL)),nn.on(/\s/,an),an.on(/[^\S\n]/,an);for(var sn=0;sn<Qt.length;sn++){var un=Yt.stateify(Qt[sn],nn,$t,Gt);Xt.push.apply(Xt,un)}var ln=Yt.stateify("file",nn,Gt,Gt),cn=Yt.stateify("ftp",nn,Gt,Gt),pn=Yt.stateify("http",nn,Gt,Gt);Xt.push.apply(Xt,ln),Xt.push.apply(Xt,cn),Xt.push.apply(Xt,pn);var fn=ln.pop(),hn=cn.pop(),gn=pn.pop(),mn=Ft(Gt),yn=Ft(Zt);hn.on("s",mn),gn.on("s",mn),Xt.push(mn),fn.on(Wt,yn),hn.on(Wt,yn),gn.on(Wt,yn),mn.on(Wt,yn);var dn=Yt.stateify("localhost",nn,Jt,Gt);for(Xt.push.apply(Xt,dn),nn.on(Bt,en),en.on("-",rn),en.on(Bt,en),en.on(Kt,on),on.on("-",rn),on.on(Kt,on),sn=0;sn<Xt.length;sn++)Xt[sn].on("-",rn),Xt[sn].on(Kt,on);rn.on("-",rn),rn.on(Bt,on),rn.on(Kt,on),nn.on(/./,Ft(It.text.SYM));var bn=function(t){for(var n=t.toLowerCase(),e=t.length,o=0,r=[];e>o;){for(var i=nn,a=null,s=0,u=null,l=-1;e>o&&(a=i.next(n[o]));)i=a,i.accepts()?(l=0,u=i):l>=0&&l++,s++,o++;0>l||(o-=l,s-=l,i=u.emit(),r.push(new i(t.substr(o-s,s))))}return r},vn=nn;Rt.State=Yt.CharacterState,Rt.TOKENS=It.text,Rt.run=bn,Rt.start=vn;var kn={__esModule:!0};kn.normalize=u,kn.resolve=l;var wn={__esModule:!0},xn=kn,On=c(xn),Ln=Rt,Sn=c(Ln),Tn=R,jn=c(Tn);Array.isArray||(Array.isArray=function(t){return"[object Array]"===Object.prototype.toString.call(t)});var zn=function(t){return jn.run(Sn.run(t))},Nn=function(t,n){for(var e=void 0===n?null:n,o=zn(t),r=[],i=0;i<o.length;i++)!o[i].isLink||e&&o[i].type!==e||r.push(o[i].toObject());return r},An=function(t,n){var e=void 0===n?null:n,o=zn(t);return 1===o.length&&o[0].isLink&&(!e||o[0].type===e)};wn.find=Nn,wn.options=On,wn.parser=jn,wn.scanner=Sn,wn.test=An,wn.tokenize=zn,window.linkify=wn}();

!function(e,t){"use strict";function n(e,t,n){var i=n[n.length-1];e.replaceChild(i,t);for(var r=n.length-2;r>=0;r--)e.insertBefore(n[r],i),i=n[r]}function i(e,t,n){for(var i=[],r=0;r<e.length;r++){var o=e[r];if(o.isLink){var a=o.toHref(t.defaultProtocol),l=f.resolve(t.format,o.toString(),o.type),s=f.resolve(t.formatHref,a,o.type),u=f.resolve(t.attributes,a,o.type),d=f.resolve(t.tagName,a,o.type),c=f.resolve(t.linkClass,a,o.type),m=f.resolve(t.target,a,o.type),y=f.resolve(t.events,a,o.type),v=n.createElement(d);if(v.setAttribute("href",s),v.setAttribute("class",c),m&&v.setAttribute("target",m),u)for(var h in u)v.setAttribute(h,u[h]);if(y)for(var k in y)v.addEventListener?v.addEventListener(k,y[k]):v.attachEvent&&v.attachEvent("on"+k,y[k]);v.appendChild(n.createTextNode(l)),i.push(v)}else i.push("nl"===o.type&&t.nl2br?n.createElement("br"):n.createTextNode(o.toString()))}return i}function r(e,t,o){if(!e||"object"!=typeof e||e.nodeType!==s)throw new Error("Cannot linkify "+e+" - Invalid DOM Node type");if("A"===e.tagName)return e;for(var a=e.firstChild;a;){switch(a.nodeType){case s:r(a,t,o);break;case u:var f=a.nodeValue,d=l(f),c=i(d,t,o);n(e,a,c),a=c[c.length-1]}a=a.nextSibling}return e}function o(e,t){var n=void 0===arguments[2]?null:arguments[2];try{n=n||window&&window.document||global&&global.document}catch(i){}if(!n)throw new Error("Cannot find document implementation. If you are in a non-browser environment like Node.js, pass the document implementation as the third argument to linkifyElement.");return t=f.normalize(t),r(e,t,n)}function a(e){function t(e){return e=o.normalize(e),this.each(function(){o.helper(this,e,n)})}var n=void 0===arguments[1]?null:arguments[1];e.fn=e.fn||{};try{n=n||window&&window.document||global&&global.document}catch(i){}if(!n)throw new Error("Cannot find document implementation. If you are in a non-browser environment like Node.js, pass the document implementation as the third argument to linkifyElement.");"function"!=typeof e.fn.linkify&&(e.fn.linkify=t,e(n).ready(function(){e("[data-linkify]").each(function(){var t=e(this),n=t.data(),i=n.linkify,r=n.linkifyNlbr,o={linkAttributes:n.linkifyAttributes,defaultProtocol:n.linkifyDefaultProtocol,events:n.linkifyEvents,format:n.linkifyFormat,formatHref:n.linkifyFormatHref,newLine:n.linkifyNewline,nl2br:!!r&&0!==r&&"false"!==r,tagName:n.linkifyTagname,target:n.linkifyTarget,linkClass:n.linkifyLinkclass},a="this"===i?t:t.find(i);a.linkify(o)})}))}var l=t.tokenize,f=t.options,s=1,u=3;o.helper=r,o.normalize=f.normalize;var d=void 0;try{d=document}catch(c){d=null}"undefined"!=typeof e&&d&&a(e,d),window.linkifyElement=o}(window.jQuery,window.linkify);

/**$( "input#msearch" )
  .keyup(function() {
    var value = $( this ).val();
    //$( "p" ).text( value );
  })
  .keyup();**/
  /*
  autogrow.js - Copyright (C) 2014, Jason Edelman <edelman.jason@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
  associated documentation files (the "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is furnished to
  do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
  LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
  */
  ;(function(e){e.fn.autogrow=function(t){function s(n){var r=e(this),i=r.innerHeight(),s=this.scrollHeight,o=r.data("autogrow-start-height")||0,u;if(i<s){this.scrollTop=0;t.animate?r.stop().animate({height:s},t.speed):r.innerHeight(s)}else if(!n||n.which==8||n.which==46||n.ctrlKey&&n.which==88){if(i>o){u=r.clone().addClass(t.cloneClass).css({position:"absolute",zIndex:-10,height:""}).val(r.val());r.after(u);do{s=u[0].scrollHeight-1;u.innerHeight(s)}while(s===u[0].scrollHeight);s++;u.remove();r.focus();s<o&&(s=o);i>s&&t.animate?r.stop().animate({height:s},t.speed):r.innerHeight(s)}else{r.innerHeight(o)}}}var n=e(this).css({overflow:"hidden",resize:"none"}),r=n.selector,i={context:e(document),animate:true,speed:200,fixMinHeight:true,cloneClass:"autogrowclone",onInitialize:false};t=e.isPlainObject(t)?t:{context:t?t:e(document)};t=e.extend({},i,t);n.each(function(n,r){var i,o;r=e(r);if(r.is(":visible")||parseInt(r.css("height"),10)>0){i=parseInt(r.css("height"),10)||r.innerHeight()}else{o=r.clone().addClass(t.cloneClass).val(r.val()).css({position:"absolute",visibility:"hidden",display:"block"});e("body").append(o);i=o.innerHeight();o.remove()}if(t.fixMinHeight){r.data("autogrow-start-height",i)}r.css("height",i);if(t.onInitialize&&r.length){s.call(r[0])}});t.context.on("keyup paste",r,s);return n}})(jQuery);
