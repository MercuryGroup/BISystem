
/**
 * @author Magnus Hernegren
 */


/**
 * options for the Spinner
 * @type {Object}
 */
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

/**
 * Whenever a navigation button is pressed
 * among the main nav buttons this method
 * is called to determine the action to be performed
 * based on the cominations of buttons that is pressed
 */
function mainNavigator() {
	if(getTopic() === 'market') {
		hide('stocklist');
		loadJSONList('list');
	} else if (getTopic() ==='stocks') {
		hide('stock');
		loadJSONList('list');
		console.log('showing stock list');
	} else if (getTopic() === 'news') {
		hide('exceptnews');
		getNewsItems('biglist');

	} else if (getTopic() === 'portfolio') {
		hide('stock');
		loadJSONList('portfolio');
	}
	spinner.stop();
}




/**
 * Loads the JSON object with all the
 * latest data from the middle layer
 * @param  {String} mode is transported to modeselector
 */
function loadJSONList(mode) {
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	var target = document.getElementById('myDiv');

	spinner = new Spinner(opts).spin(target);
try {getStockData();

	modeSelector(mode);
}
catch (e) {
	$.getJSON(
		// 'tempj/nyse'
		'/couchdb/mercury_latest/_design/bi/_view/stock'
		// 'http://mercury.dyndns.org:5984/mercury_latest/_design/bi/_view/stock'
		, function(url_data) {
			$.each(url_data, function (i,element) {
				if ($.isArray(element) === true) {
					// sortArrayBySymbol(element);
					setStockData(element);
					modeSelector(mode);
				} 
			});
		});
}
}

 
/**
 * Navigates to the correct mode 
 * @param  {String} mode determines the method to be invoked
 */
function modeSelector(mode) {
	if (mode === 'search') {
		search();
	} else if (mode === 'list' && getTopic() === 'stocks') {
		loadSingleStockList();
	} else if (mode === 'list' && getTopic() === 'market') {
		var cn = document.getElementById('companyname').innerHTML=getMarket();
		setChartType("line");
		hide('list');
		firstDataFill = true;
		setSymbol('NYSE');
		setStockMode('market');
		gethistory('month');
		getNewsItems('biglist');
	} else if (mode === 'list' && getTopic() === 'news') {

	} else if (mode === 'portfolio') {
		portfoliobuilder();
	}
	spinner.stop();
}


/**
 * Runs through the stock array and filters
 * out the stocks for the market that is selected
 */
function loadSingleStockList() {
	try {getStockData()}
	catch (e) {
		loadJSONList();
	}
	var tableRef = document.getElementById('resultList');
	var otherLetter = null;
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	var target = document.getElementById("letternavigator");
	document.getElementById("letternavigator").innerHTML = "";
	var firstInList = null;

	$.each(getStockData(), function (i,value) {

		if (getMarket() === value.value.market) {
			console.log(getMarket() === value.value.market);
			setFirstLetter(value.value.symbol.substr(0,1));
			if(firstInList === null)  {
				firstInList = getFirstLetter();
			}
			if (getFirstLetter() !== otherLetter) {
				otherLetter = getFirstLetter();
				addSearchLetters(getFirstLetter());
			} 
		}	
	});
	addPageNumbers(firstInList);
}      

/**
 * Searches through the array of stocks when the user uses the search function
 */
function search() {
	document.getElementById('naviInfo').innerHTML = "Search Results";
	var target = document.getElementById("letternavigator").innerHTML = '';
	var target = document.getElementById("pagenavigator").innerHTML = '';
	try {getStockData()}
	catch (e) {
		loadJSONList();
	}
	var target = document.getElementById('myDiv');
	// var spinner = new Spinner(opts).spin(target);
	var tableRef = document.getElementById('resultList');
	var searchterm = document.getElementById("sok").value.toLowerCase();
	hide('stock');
	// spinner.spin();
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	console.log("Searching for "+searchterm);

	$.each(getStockData(), function (i,value) {
		if (value.value.symbol.toLowerCase().indexOf(searchterm) != -1 || value.value.name.toLowerCase().indexOf(searchterm) != -1) {
			console.log("Found Match");
			addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest,value.value.market);    
		}
	});
}    

/**
 * Builds a portfolio with only saved stocks, checks the array
 * of stocks and compares them to the localstorage saved ones,
 * if one is found, it adds it to a table via addElement.
 */
function portfoliobuilder() {
	var target = document.getElementById("letternavigator").innerHTML = '';
	var target = document.getElementById("pagenavigator").innerHTML = '';
	var tableRef = document.getElementById('resultList');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	try {getStockData()}
	catch (e) {
		loadJSONList();
	}

	$.each(getStockData(), function (i,value) {
		if (localStorage[value.value.symbol] !== undefined) {
			addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest,value.value.market);    
		}
	});
}      

/**
 * Adds a button with a letter for the letternavigation
 * @param {String} letter The letter which will be the button
 */
function addSearchLetters(letter) {

	var target = document.getElementById("letternavigator");
	var button = document.createElement('button');
	var tnode = document.createTextNode(letter);
	button.setAttribute('onClick', 'addPageNumbers("'+letter+'");');
	button.appendChild(tnode);
	target.appendChild(button);
}

/**
 * Adds the appropriate number of pages to the pagenavigator
 * for the selected letter
 * @param {String} letter The letter for which pages should be created
 */
function addPageNumbers(letter) {
	var target = document.getElementById("pagenavigator");
	document.getElementById("pagenavigator").innerHTML = "";
	var count = 0;
	var pcount =1;
	$.each(getStockData(), function (i,value) {
		if (letter === value.value.symbol.substr(0,1) && count === 0 && getMarket() === value.value.market) {
			setFirstLetter(letter);

			var target = document.getElementById("pagenavigator");
			var button = document.createElement('button');
			var tnode = document.createTextNode(pcount);
			button.setAttribute('onClick', 'addElementsForPage("'+pcount+'");');
			button.appendChild(tnode);
			target.appendChild(button);
			pcount++;
			count++;
		} else if (letter === value.value.symbol.substr(0,1) && count === 20 && getMarket() === value.value.market) {
			count = 0;

		} else if (letter === value.value.symbol.substr(0,1) && getMarket() === value.value.market) {
			count++;
		}

	});
	addElementsForPage(1);

}

/**
 * Traverses the stock list and checks for stocks
 * which start with the getFirstLetter() and belongs to the page number
 * @param {Integer} pcount Is the specific page for which to add stocks
 */
function addElementsForPage(pcount) {
	var tableRef = document.getElementById('resultList');
	var otherLetter = null;
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	var count = 0;
	var elementrange = pcount * 20;
	var startvalue = elementrange - 20;
	$.each(getStockData(), function (i,value) {
		if (getFirstLetter() === value.value.symbol.substr(0,1) && count < startvalue+20 && count >= startvalue && getMarket() === value.value.market) {
			count++;
			addElement(value.value.symbol,value.value.name,value.value.change,value.value.latest,value.value.market); 
		} else if(getFirstLetter() === value.value.symbol.substr(0,1) && getMarket() === value.value.market) {count++;}
	});
}
/**
 * Adds a clickable row for each stock it is passed
 * @param {String} Symbol Symbol of the stock
 * @param {String} Name   Name of the company
 * @param {String} Change Change value
 * @param {String} Value  Value of the stock
 * @param {String} Market Market for the stock
 */
function addElement(Symbol,Name,Change,Value,Market) {
	var cdata = [Symbol,Name,Change,Value];
	var ni = document.getElementsByTagName('tbody').item(0);
	var newrow = document.createElement('tr');
	newrow.className='clickableRow';
	newrow.onclick = function() {
		setSymbol(Symbol);
		setMarket(Market);
		hide('stocklist');
		setChartType("line");
		firstDataFill = true;
		setStockMode('stock');
		gethistory('month');
		var cn = document.getElementById('companyname').innerHTML=Name;
		getNewsItems('null');
		portfolioController();
		paintlinechart();
	}
	for (var i = 0; i<cdata.length;i++) {
		newrow.appendChild(makecells(cdata[i]));
	}
	ni.appendChild(newrow);
}

/**
 * Cell creator for tables
 * @param  {String} cdata String to be placed in textnode in cell
 * @return {td}      a td element with a appended textnode
 */
function makecells(cdata) {
	cell = document.createElement("td");
	textnode = document.createTextNode(cdata);
	cell.appendChild(textnode);
	return cell;
}




/**
 * This method gets the selected history for a specific stock
 * The stocks that match the symbol, market and time selected are 
 * added to an array.
 * @param  {String} timeframe Decides which timeframe to get for the stock
 */
function gethistory(timeframe) {
	duration = timeframe;
	today = Date.now().valueOf();
	if (timeframe === "week") {
		timeframe = Date.today().add(-7).days();
		timeframe = timeframe.valueOf();
		console.log(timeframe);
	}
	if (timeframe === "month") {
		timeframe = Date.today().add(-31).days();
		timeframe = timeframe.valueOf();

		console.log(timeframe);
	} if (timeframe === "today") {
		timeframe = Date.today();
		timeframe = timeframe.valueOf();

		console.log(timeframe);
	}
	console.log("Gethistory mode is "+mode);
	if (getStockMode() === 'stock') {

		setURL(
			 '/couchdb/mercury/_design/bi/_view/'
			 // 'http://mercury.dyndns.org:5984/mercury/_design/bi/_view/'
			 +getMarket().toLowerCase()+'_stock?startkey=[%22'+getSymbol()+'%22,%22'+timeframe+'%22]&endkey=[%22'+getSymbol()+'%22,%22'+today+'%22]');
	} else if (getStockMode() === 'market') {

		setURL(
			 '/couchdb/mercury/_design/bi/_view/'
			 // 'http://mercury.dyndns.org:5984/mercury/_design/bi/_view/'
			 +getMarket().toLowerCase()+'_market?startkey=%22'+timeframe+'%22&endkey=%22'+today+'%22');
	}

	var tableRef = document.getElementById('resultList');
	var date1 = null;
	var date2 = null;
	dailyValues = [];
	dayLow = [];
	dayHigh = [];
	var Dat;
	var OVal;
	var Chan;
	var Lat;
	var nextDay = new Boolean();
	var firstDay = new Boolean();
	var i = 0;
	diadata = [];
	console.log("History url "+getURL());
	firstDay = true;
	$.getJSON(getURL(), function(url_data) {
			// $.getJSON('tempj/ABXday', function(url_data) {

				$.each(url_data, function (i,element) {
					if ($.isArray(element) === true) {
						sortArrayByTime(element);
						element.reverse();
							if (firstDataFill === true) {
								fillInDataTable({date: element[0].value.updated,c: element[0].value.latest,o: element[0].value.openVal,cl: element[0].value.change,vol: element[0].value.volume,percent: element[0].value.percent});
								firstDataFill = false;
							}

						$.each(element, function (i,value) {

							dayLow = [];
							dayHigh = [];
							if (duration === "today") {
								console.log("Getting data for day");
								var date = new Date(parseInt(value.value.updated));
								date2=date.toString("HH:mm");
								Dhi = null;
								Dlow = null;
								diadata.push({date: date2,c: parseFloat(value.value.latest),o: parseFloat(value.value.openVal),h: Dhi,l:Dlow,cl: parseFloat(value.value.change),vol: value.value.volume,percent: value.value.percent});
							} 
							if (duration === "month" || duration === "week") {
								date2 = dateConvert(value.value.updated);
								if (date1 === date2) {
									dailyValues.push(parseFloat(value.value.latest));
									nextDay = true;
								} else {
									if (nextDay === true) {
										dailyValues.sort();
										Dlow = dailyValues[0];
										Dhi = dailyValues.pop();
										dailyValues = [];
										nextDay = false;
										diadata[i-1] = {date: Dat,c: Lat,o: OVal,h: Dhi,l:Dlow,cl:Chan};
										firstDay = false;
									}
									date1 = date2;		
									OVal = parseFloat(value.value.openVal);
									Dat =date2; 
									Chan = parseFloat(value.value.change);
									Lat = parseFloat(value.value.latest);
									dailyValues.push(parseFloat(value.value.openVal));
									dailyValues.push(parseFloat(value.value.latest));
									dailyValues.sort();
									Dlow = dailyValues[0];
									Dhi = dailyValues[dailyValues.length-1];
									if (firstDay === true) {
										diadata[i]={date: Dat,c: Lat,o: OVal,h: Dhi,l:Dlow,cl:Chan};
										i++;
									}
									firstDay = true;
								}
							}
						});
}
});
diadata3 = [];
for (var i = 0;i<diadata.length;i++) {
	if (diadata[i] !==  undefined) { 
		diadata3.push(diadata[i]);
	}
}
setDiadata(diadata3.reverse());
chartPaintSelector();

});

}    


/**
 * Converts miliseconds to a date
 * @param  {String} timeString date in milliseconds
 * @return {String}            date in "MMM dd" format
 */
function dateConvert(timeString) {
	var date = new Date(parseInt(timeString));
	date=date.toString("MMM dd");
	return date;
}



/**
 * Gets news items for the selected stock or for a market
 * @param  {string} mode determines if the method should get 
 * a big list for a market or a small list for a specific stock
 */
function getNewsItems(mode) {

	var today = Date.now().valueOf();
	var timeframe = Date.now().add(-48).hours();
	var timeframe = timeframe.valueOf();
	// var symbol = getSymbol();
	var nitem = 0;
	var tableRef = document.getElementById('newstable');
	while ( tableRef.rows.length > 0 )
	{
		tableRef.deleteRow(0);
	}
	if(mode === 'biglist') {
		maxitems = 100;
		document.getElementById('newsdisplay').style.height = '600px';
		setURL(
			'/couchdb/mercury/_design/bi/_view/news_list?startkey=%22'
			// 'http://mercury.dyndns.org:5984/mercury/_design/bi/_view/news_list?startkey=%22'
			+timeframe+'%22&endkey=%22'+today+'%22');
	} else {
		maxitems = 10;
					setURL(
						'/couchdb/mercury/_design/bi/_view/news?key=[%22'
						// 'http://mercury.dyndns.org:5984/mercury/_design/bi/_view/news?key=[%22'
						+getSymbol()+'%22,%22'+getMarket()+'%22]');
				}
				console.log(getURL());
				$.getJSON(getURL(), function(url_data) {
		// $.getJSON('http://mercury.dyndns.org:5984/mercury/_design/bi/_view/news_list?startkey=%22'+timeframe+'%22&endkey=%22'+today+'%22', function(url_data) {
			$.each(url_data, function (i,element) {
				if ($.isArray(element) === true) {
					sortNewsArrayByTime(element);
					element.reverse();
					$.each(element, function (i,value) {
						if(mode==='biglist' && nitem < maxitems && value.value.market === getMarket()) {
							addNewsListItem({title: value.value.title,link: value.value.link,description: value.value.description,date: value.value.pubDate});
							nitem++;
						}
						if (nitem < maxitems && mode != 'biglist') {
							addNewsListItem({title: value.value.title,link: value.value.link,description: value.value.description,date: value.value.pubDate});
							nitem++;
						}
					});
				}
			});
		});
			}

/**
 * Adds a header and date as a tr element to the news table
 * @param {Object} newsitem Is an news object containing news information
 */
function addNewsListItem(newsitem) {
	var date2 = new Date(parseInt(newsitem.date));
	date2 = date2.toString("MMM dd HH:mm");
	var cdata = [newsitem.title,date2];
	var newstable = document.getElementById('newstable');
	var newrow = document.createElement('tr');
	newrow.className='clickableRow';
	newrow.onclick = function() {
		setNewsItem(newsitem);
		hide('news');
	};
	for (var i = 0; i<cdata.length;i++) {
		newrow.appendChild(makecells(cdata[i]));
	}
	newstable.appendChild(newrow);
}

/**
 * Fills the table that displays todays data
 * @param  {Object} diadata An object containing todays data
 */
function fillInDataTable(diadata) {
	var diadata = diadata
	$(document).ready(function() {
		$("#dailystats").find("tr:gt(0)").remove();
	});

	var ni = document.getElementById('dailystats');
	var newrow = document.createElement('tr');
	console.log(dateConvert(Date.today().valueOf()) +" date2" + dateConvert(diadata.date));
	if (dateConvert(Date.today().valueOf()) === dateConvert(diadata.date)) {
		var cdata =[getSymbol(),
		diadata.c,
		diadata.cl,
		diadata.percent,
		diadata.o,
		diadata.vol];
	} else {
		var cdata = [getSymbol(),"Market closed","Market closed","Market closed","Market closed","Market closed"];
	}

	for (var i = 0; i<cdata.length;i++) {
		newrow.appendChild(makecells(cdata[i]));
	}

	console.log(document.getElementById('latest'));
	ni.appendChild(newrow);
}



/**
 * Checks wether the stock is already saved or not upon loading a stock,
 * also deletes and saves stocks upon pressing the save button.
 */
function portfolioController() {
	var sb = document.getElementById('save');
	if (localStorage[getSymbol()] !== undefined) {
		document.getElementById('save').innerHTML = "Remove from portfolio";
	} else  {
		document.getElementById('save').innerHTML = "Add to portfolio";
	}
	sb.onclick = function() {
		if (localStorage[getSymbol()] !== undefined) {
			localStorage.removeItem(getSymbol());
			document.getElementById('save').innerHTML = "Add to portfolio";
		} else {
			localStorage[getSymbol()] = getSymbol();
			document.getElementById('save').innerHTML = "Remove from portfolio";
		}
	}
}



/**
 * Hides irrelevant items,menus,etc
 * and makes sure only what is supposed to be displayed is.
 * @param  {String} item A keyword on what element should be hidden
 */
function hide(item) {
	var nitem = document.querySelector("#newsdisplay");
	var slist = document.querySelector("#listdisp");
	var chart = document.querySelector("#statView");
	var nlist = document.querySelector("#stocknews");
	var nitem = document.querySelector("#newsdisplay");
	if (item === 'stocklist') {

		slist.className = 'visuallyhidden';
		chart.className = 'visible';
		nlist.className = 'visible';
	} else if (item === 'news') {
		nlist.className = 'visuallyhidden';
		nitem.className = 'visible';
		var newslink = document.getElementById('newslink');
		newslink.href = getNewsItem().link;
		var nheadline = document.getElementById('newsheadline').innerHTML = getNewsItem().title;
		var newsdisplay = document.getElementById('newstext').innerHTML = getNewsItem().description;
	} else if (item === 'newsitem') {
		nitem.className = 'visuallyhidden';
		nlist.className = 'visible';
	} else if (item === 'exceptnews') {
		slist.className = 'visuallyhidden';
		nitem.className = 'visuallyhidden';
		nlist.className = 'visible';
		chart.className = 'visuallyhidden';
	} else if (item === 'stock') {
		chart.className = 'visuallyhidden';
		slist.className = 'visible';
		nlist.className = 'visuallyhidden';
		nitem.className = 'visuallyhidden';
	}
}


/**
 * Sorting functions for the JSON object
 * @param  {Array} data Array of JSON objects to be sorted
 * @return {Array} Array Sorted array
 */
function sortArrayBy(data) {
	data.sort(function (a, b) {
		a = a.value.updated;
		b = b.value.updated;
		return a.localeCompare(b);
	});
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


function sortNewsArrayByTime(data) {
	data.sort(function (a, b) {
		a = a.value.pubDate;
		b = b.value.pubDate;
		return a.localeCompare(b);
	});
}

/**
 * Paints a line chart showing closing values,
 * uses getDiadata() for chart data
 */
function paintlinechart() {
	{
		$("#canvas").dxChart({
			title: {
				text: 'Value'
				// placeholderSize: 20
			},
			legend: {
				verticalAlignment: "bottom",
				visible:false,
			},
			dataSource: getDiadata(),

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


/**
 * Paints a candlestick diagram,
 * uses getDiadata() for chart data
 */
function paintCandlestick() {
	$("#canvas").dxChart({
		title: "Value",
		dataSource: getDiadata(),
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

/**
 * Paints a bar chart showing the change over the selected time,
 * uses getDiadata() for chart data
 */
 function paintbarchart() {
	{
		$("#canvas").dxChart({
			title: {
				text: 'Change'
			},
			dataSource: getDiadata(),

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

/**
 * Paints a chart depending on what getChartType() currently is
 * 
 */
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