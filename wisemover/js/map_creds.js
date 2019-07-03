var map = L.map('map',{
	zoomControl: false
	
}).setView([53.802, -1.553], 13);

L.control.zoom({position: 'topright'}).addTo(map);


L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
		maxZoom: 18,
		attribution: 'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, ' +
			'<a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
			'Imagery ? <a href="https://www.mapbox.com/">Mapbox</a>',
		id: 'mapbox.streets'
}).addTo(map);



function getColor(d) {
	return 	d > 90  ? '#67001f' :
			d > 80  ? '#b2182b' :
			d > 70  ? '#d6604d' :
			d > 60  ? '#f4a582' :
			d > 50  ? '#fddbc7' :
			d > 40  ? '#d1e5f0' :
			d > 30  ? '#92c5de' :
			d > 20  ? '#4393c3' :
			d > 10  ? '#2166ac' :
					  '#053061';
}
		
function style(feature) {
	return {
		fillColor: getColor(feature.properties.val),
		weight: 0.5,
		opacity: 0,
		fillOpacity: 0.7
	};
}


var zones = L.geoJson(null);
//var pricevalues = $('form').serialize();
//var pricevalues = $('priceform');
//var crimevalues = $('crimeform').serialize();
//var pricevalues = document.getElementById("maxprice").value;
//console.log(pricevalues);

map.on('moveend', function onDragEnd(){
zones.clearLayers();
var pricevalues = $('form').serialize();
//console.log(pricevalues);

$.ajax({
    		type: "GET",
     		url: "https://www.wisemover.co.uk/api/creds_public.php?bbox=" + map.getBounds().toBBoxString() + '&' + pricevalues,
    		dataType: 'json',
    		success: function (response) {
        		zones = L.geoJson(response, {style: style}, {
            			onEachFeature: function (feature, layer) {
            			layer.bindPopup(feature.properties.val);
            			}
        	});
     	zones.addTo(map);
	 	}});
});