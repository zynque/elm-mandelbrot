var canvas = document.getElementById('elm-canvas');
var ctx = canvas.getContext('2d');

function updateImageData(i) {
    var imageData = ctx.createImageData(i.width,i.height);
    imageData.data.set(i.data)
    ctx.putImageData(imageData, i.x, i.y);
}
