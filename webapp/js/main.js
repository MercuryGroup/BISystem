var opts = {
  lines: 11, // The number of lines to draw
  length: 22, // The length of each line
  width: 8, // The line thickness
  radius: 19, // The radius of the inner circle
  corners: 0.6, // Corner roundness (0..1)
  rotate: 36, // The rotation offset
  direction: 1, // 1: clockwise, -1: counterclockwise
  color: '#000', // #rgb or #rrggbb or array of colors
  speed: 1.7, // Rounds per second
  trail: 32, // Afterglow percentage
  shadow: true, // Whether to render a shadow
  hwaccel: false, // Whether to use hardware acceleration
  className: 'spinner', // The CSS class to assign to the spinner
  zIndex: 2e9, // The z-index (defaults to 2000000000)
  top: 90, // Top position relative to parent in px
  left: 'auto' // Left position relative to parent in px
};




function jsonparser() {
	var target = document.getElementById('myDiv');
	var spinner = new Spinner(opts).spin(target);
	var tableRef = document.getElementById('resultList');
	var otherLetter = null;
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
		var target = document.getElementById("letternavigator");
	document.getElementById("letternavigator").innerHTML = "";
	var firstInList = null;
	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				setStockData(element);
				console.log("getstockdata name " +getStockData()[1].value.name);
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					setFirstLetter(value.value.symbol.substr(0,1));
					if(firstInList === null)  {
						firstInList = getFirstLetter();
					}
					if (getFirstLetter() !== otherLetter) {
						otherLetter = getFirstLetter();
						addSearchLetters(getFirstLetter());
					} else {

						// otherLetter
					}
					
				});
			}
		});
		addPageNumbers(firstInList);
		spinner.stop();
	});
	
}      

function addSearchLetters(letter) {

	var target = document.getElementById("letternavigator");
	var button = document.createElement('button');
	var tnode = document.createTextNode(letter);
	button.setAttribute('onClick', 'addPageNumbers("'+letter+'");');
	button.appendChild(tnode);
	target.appendChild(button);
}

function addPageNumbers(letter) {

	var target = document.getElementById("pagenavigator");
	document.getElementById("pagenavigator").innerHTML = "";
	var count = 0;
	var pcount =1;
	$.each(getStockData(), function (i,value) {
		if (letter === value.value.symbol.substr(0,1) && count === 0) {
			setFirstLetter(letter);

			var target = document.getElementById("pagenavigator");
			var button = document.createElement('button');
			var tnode = document.createTextNode(pcount);
			button.setAttribute('onClick', 'addElementsForPage("'+pcount+'");');
			button.appendChild(tnode);
			target.appendChild(button);
			pcount++;
			count++;
				// addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest); 
			} else if (letter === value.value.symbol.substr(0,1) && count === 20) {
				count = 0;

			} else if (letter === value.value.symbol.substr(0,1)) {
				count++;
			}

		});
				addElementsForPage(1);

}
function addElementsForPage(pcount) {
	var tableRef = document.getElementById('resultList');
	var otherLetter = null;
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}

	var count = 0;
	var elementrange = pcount * 20
	var startvalue = elementrange - 20;
	$.each(getStockData(), function (i,value) {
		if (getFirstLetter() === value.value.symbol.substr(0,1) && count < startvalue+20 && count >= startvalue) {
			count++;
			addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest); 
		} else if(getFirstLetter() === value.value.symbol.substr(0,1)) {count++;}
	});
	console.log(count);

}

function search() {
	var target = document.getElementById('myDiv');
	var spinner = new Spinner(opts).spin(target);
	var tableRef = document.getElementById('resultList');
	var searchterm = document.getElementById("sok").value.toLowerCase();
	spinner.spin();
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	console.log("Searching for "+searchterm);
	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					if (value.value.symbol.toLowerCase().indexOf(searchterm) != -1 || value.value.name.toLowerCase().indexOf(searchterm) != -1) {
						console.log("Found Match");
						addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
					} else {
				// console.log("No Match "+ value.value.symbol);
			}
		});
			}
		});
		spinner.stop();
	});
}    


function portfoliobuilder() {
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}

	$.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=%221384142400000%22&endkey=%221384172149000%22', function(url_data) {
		$.each(url_data, function (i,element) {
			if ($.isArray(element) === true) {
				sortArrayBySymbol(element);
				$.each(element, function (i,value) {
					if (isStockSaved(value.value.symbol) === true) {
						addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest);    
					}
				});
			}
		});
	});
}      




function getstockhistory(symbol,timeframe,name) {
	setName(name);
	duration = timeframe;
	today = Date.now().valueOf();
	
	if (timeframe === "week") {
		timeframe = Date.today().add(-7).days();
		timeframe = timeframe.valueOf();
		timeframe = 'week'
		console.log(timeframe);
	}
	if (timeframe === "month") {
		timeframe = Date.today().add(-30).days();
		timeframe = timeframe.valueOf();
		timeframe = 'month'
		console.log(timeframe);
	} if (timeframe === "today") {
		// timeframe = Date.today();
		// timeframe = timeframe.valueOf();
		timeframe = 'day'
		console.log(timeframe);
	}

	var tableRef = document.getElementById('resultList');
	var date1 = null;
	var date2 = null;
	dailyValues = [];
	datelist = [];
	openVallist = [];
	latestlist = [];
	changelist = [];
	percentlist = [];
	dayLow = [];
	dayHigh = [];
	diadata = [];
	var Dat;
	var OVal;
	var Chan;
	var Lat;
	var nextDay = new Boolean();
	diadata = [];
	console.log('http://mercury.dyndns.org:8080/JAXRS-BISystem/api/stocks/'+timeframe+'/'+symbol);
	// $.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse_stock?startkey=[%22'+symbol+'%22,%22'+timeframe+'%22]&endkey=[%22'+symbol+'%22,%22'+today+'%22]', function(url_data) {
		$.getJSON('http://mercury.dyndns.org:8080/JAXRS-BISystem/api/stocks/'+timeframe+'/'+symbol, function(url_data) {
			sortArrayByTime(url_data);
			url_data.reverse();
			setStockData(url_data);
			$.each(url_data, function (i,element) {
				console.log(element.value.name);

			// if ($.isArray(element) === true) {


				// console.log(getStockData()[1]);

					// $.each(getStockData(), function (i,value) {
						dayLow = [];
						dayHigh = [];

						if (timeframe === "day") {
							console.log("Getting data for day");
							
							var date = new Date(parseInt(element.value.updated));
							date2=date.toString("HH:mm");

							openVallist.push(element.value.openVal);

							datelist.push(date2); 
							latestlist.push(element.value.latest);

							OVal = parseFloat(element.value.openVal);
							Dat =date2; 
							Chan = parseFloat(element.value.change);
							Lat = parseFloat(element.value.latest);
							Dhi = null;
							Dlow = null;
							console.log({date: Dat,c: Lat,o: OVal,h: Dhi,l:Dlow,cl: Chan});
							diadata.push({date: Dat,c: Lat,o: OVal,h: Dhi,l:Dlow,cl: Chan});
						// changelist.push(parseFloat(value.value.change));
					} 
					if (timeframe === "month" || timeframe === "week") {
						date2 = dateConvert(element.value.updated);
						if (date1 === null) {


						}
						if (date1 === date2) {
							dailyValues.push(parseFloat(element.value.latest));
							nextDay = true;
						} else {
							if (nextDay === true) {
								dailyValues.sort();

								Dlow = dailyValues[0];
								Dhi = dailyValues.pop();
								dailyValues = [];
								nextDay = false;
								diadata.push({date: Dat,c: Lat,o: OVal,h: Dhi,l:Dlow,cl:Chan});

							}

							date1 = date2;		
							OVal = parseFloat(element.value.openVal);
							Dat =date2; 
							Chan = parseFloat(element.value.change);
							Lat = parseFloat(element.value.latest);
							dailyValues.push(parseFloat(element.value.openVal));
							dailyValues.push(parseFloat(element.value.latest));



						}

					}


				// });
// }
});

diadata.reverse();
setOpenVallist(openVallist.reverse());
setDateList(datelist.reverse());
setLatestList(latestlist.reverse());
setChangeList(changelist.reverse());
chartPaintSelector();
if (firstDataFill === true) {
	fillInDataTable();
	firstDataFill = false;
}
});

}    



function dateConvert(timeString) {
	var date = new Date(parseInt(timeString));
	date=date.toString("MMM dd");
	return date;
}




function addElement(Symbol,Name,Change,Value) {
	var ni = document.getElementsByTagName('tbody').item(0);
	var numi = document.getElementById('theValue');
	var num = (document.getElementById('theValue').value -1)+ 2;
	numi.value = num;
	var newdiv = document.createElement('tr');
	newdiv.className='clickableRow';
	newdiv.setAttribute('onClick', 'hidelist("'+Symbol+'","today","'+Name+'");');
	// newdiv.addEventListener('click', hidelist(Symbol));
	cell = document.createElement("td");
	cellName = document.createElement("td");
	cellChange = document.createElement("td");
	cellValue = document.createElement("td");
	textnode = document.createTextNode(Symbol);
	textName = document.createTextNode(Name);
	textChange = document.createTextNode(Change);
	textValue = document.createTextNode(Value);
	cell.appendChild(textnode);
	cellName.appendChild(textName);
	cellChange.appendChild(textChange);
	cellValue.appendChild(textValue);
	newdiv.appendChild(cell);
	newdiv.appendChild(cellName);
	newdiv.appendChild(cellChange);
	newdiv.appendChild(cellValue);
	ni.appendChild(newdiv);
}


function fillInDataTable() {
	portfolioController();
	var tableRef = document.getElementById('currentdata');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	var cn = document.getElementById('companyname').innerHTML=getName();
	var ni = document.getElementById('currentdata');
	var numi = document.getElementById('theValue');
	var num = (document.getElementById('theValue').value -1)+ 2;
	numi.value = num;
	var newdiv = document.createElement('tr');
	newdiv.className='clickableRow';

	try  {
		Change = diadata[diadata.length-1].cl;
		Value = diadata[diadata.length-1].c;

	} catch (te) {

		Change = "Market closed";
		Value = "Market closed";
	}
	cell = document.createElement("td");
	cellName = document.createElement("td");
	cellChange = document.createElement("td");
	cellValue = document.createElement("td");
	cellSave = document.createElement("td");
	textnode = document.createTextNode(Symbol);
	textName = document.createTextNode(getName());
	textChange = document.createTextNode(Change);
	textValue = document.createTextNode(Value);
	textSave = document.createTextNode("+");
	cellSave.setAttribute('onClick', 'hidelist("'+Symbol+'","today");');
	cell.appendChild(textnode);
	cellName.appendChild(textName);
	cellChange.appendChild(textChange);
	cellValue.appendChild(textValue);
	cellSave.appendChild(textSave);
	newdiv.appendChild(cell);
	newdiv.appendChild(cellChange);
	newdiv.appendChild(cellValue);
	ni.appendChild(newdiv);
}



function portfolioController() {
	var sb = document.getElementById('save');
	if (isStockSaved(Symbol) === true) {
		console.log("Already in portfolio");
		sb.onclick = function() { deleteStock(Symbol);
			document.getElementById('save').innerHTML = "+";
		}
		document.getElementById('save').innerHTML = "Remove";
	} else {
		console.log("Not saved");
		sb.onclick = function() { saveStock(Symbol);
			document.getElementById('save').innerHTML = "Remove";

		}
		document.getElementById('save').innerHTML = "+";
	}
}


function isStockSaved(symbol) {
	if (localStorage[symbol] === undefined) {

		return false;
	}
	else {

		return true;
	}
}

function saveStock(symbol) {
	localStorage[symbol] = symbol;
	console.log("saved");
	portfolioController();
}

function deleteStock(symbol) {
	localStorage.removeItem(symbol);
	console.log("deleted "+isStockSaved(Symbol));
	portfolioController();
}


function paintlinechart() {
	console.log("painting line chart with data "+ diadata[1].date);

	{
		$("#canvas").dxChart({
			title: {
				text: 'Stock Price'
			},
			legend: {
				verticalAlignment: "bottom",
				visible:false,
			},
			dataSource: diadata,

			valueAxis: {
				title: { 
					text: "EUR"
				},

			},

			series: {
				argumentField: "date",
				valueField: "c",
				color: 'rgba(220,53,34,0.7)',
				type: "line",
				point: {
					color: 'rgba(220,53,34,1)'
				}
			},

			tooltip:{
				enabled: true
			}
		});
	}
}

function paintCandlestick() {
	diagramdata = [];
	dayHigh.reverse();
	dayLow.reverse();
	console.log("painting candlestick");
	for (var i = 0;i <= getDateList().length;i++) {
		diagramdata.push({date: getDateList()[i],c: getLatestList()[i],o: openVallist[i],h: dayHigh[i],l:dayLow[i]});
		console.log("diagramdata " +diagramdata[i] +" daylow " +dayLow[i]  );
	}

	$("#canvas").dxChart({
		title: "Stock Price",
		dataSource: diadata,
		commonSeriesSettings: {
			argumentField: "date",
			type: "candlestick"
		},
		series: [
		{ 
			name: "STOCKS",
			openValueField: "o", 
			highValueField: "h", 
			lowValueField: "l", 
			closeValueField: "c", 
			reduction: {
				color: 'rgba(220,53,34,0.9)'
			}
		}
		],    
		valueAxis: {
			title: { 
				text: "EUR"
			},

		},
		tooltip:{
			enabled: true
		},
		argumentAxis: {
			label: {
				format: "shortDate"
			}
		}
	});
}


function paintbarchart() {
	var cl = getChangeList();
	diagramdata = [];
	for (var i = 0;i <= cl.length;i++) {
		diagramdata.push({day: getDateList()[i],value: getChangeList()[i]});
	}
	
	{
		$("#canvas").dxChart({
			title: {
				text: 'Change'
			},
			dataSource: diadata,

			valueAxis: {
				title: { 
					text: "EUR"
				},

			},

			series: {
				argumentField: "date",
				valueField: "cl",
				name: "The daily change",
				type: "bar",
				color: 'rgba(220,53,34,0.9)',
			}
		});
	}
}


function chartPaintSelector() {
	console.log("chartPaintSelector "+getChartType());
	if(getChartType() === "bar") {
		paintbarchart();
	}
	if (getChartType() === "line") {
		paintlinechart();
	}
	if (getChartType() === "candle") {
		paintCandlestick();
	}
}
function hideLine() {
	paintbarchart();

}



function hidelist(Symbol,timeframe,name) {
	setSymbol(Symbol);
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	con.className = 'visuallyhidden';
	chart.className = 'visible';
	radiobtn = document.getElementById("day");
	radiobtn.checked = true;
	setChartType("line");
	firstDataFill = true;
	getstockhistory(Symbol,timeframe,name);


}


function showlist() {
	con = document.querySelector("#listdisp");
	chart = document.querySelector("#statView");
	chart.className = 'visuallyhidden';
	con.className = 'visible';

}

function sortArrayByTime(data) {
	data.sort(function (a, b) {
		a = a.value.updated;
		b = b.value.updated;
		return a.localeCompare(b);
	});
}

function sortArrayBySymbol(data) {
	data.sort(function (a, b) {
		a = a.value.symbol;
		b = b.value.symbol;
		return a.localeCompare(b);
	});
}

