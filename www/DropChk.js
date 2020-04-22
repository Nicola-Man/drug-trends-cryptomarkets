var Amark = document.getElementById("AMarket");
var Adrug = document.getElementById("ADrug");
var Tmark = Amark.querySelectorAll("span");
//seems not to like this
//const drop = document.getElementByName("Drop");
var rad = document.getElementById("Drop");
//rad.childNodes[1].innerHTML="Plot:"
//rad.childNodes[0:4] [object Text][object HTMLLabelElement][object Text][object HTMLDivElement][object Text]
var radval = rad.querySelectorAll("input[name='Drop']");

//Didn't work???
// ChkIndet(Amart);
// ChkIndet(Adrug);
// function ChkIndet(chk) {
//     chk.indeterminate = true;
ChkIndet();
function ChkIndet() {
    Amark.indeterminate = true;
    Adrug.indeterminate = true;
    // document.getElementById("check").innerHTML = ("check " + Tmark.textContent);
}

// ChkHide();

// function ChkHide() {
//     if (radval[0].checked==true) {
//         Amark.style.display = 'none'  ; 
//         Tmark.textContent = null  ; 
//     }
//     else {
//         Amark.style.display = 'hidden'  ;
//         Tmark.textContent = "sfdghgf"  ; 
//     }
// document.getElementById("event").innerHTML = ("event " + radval[0].checked);
// }

// for (x = 0; x < radval.length; x++) {
//     radval[x].addEventListener('change', ChkHide);
//     document.getElementById("length").innerHTML = ("length " + radval[x].checked);
// }