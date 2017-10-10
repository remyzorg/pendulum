var timerID = null;
var elapsedTime = 0;
function doEverySecond() {
    elapsedTime += 1;
    document.getElementById("curTime")
        .innerHTML = elapsedTime;
}
function startTimer() {
    timerId = setInterval("doEverySecond()", 1000);
}
function resetElapsed() {
    elapsedTime = 0;
}
