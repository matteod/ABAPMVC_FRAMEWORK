/*** BOF ***/

/************************** CONFIG SECTION **************************/
//var sap_s_account = (typeof(window.sap_s_account)=='string')?window.sap_s_account:'sapscndev';
var sap_s_account = (typeof(window.sap_s_account)=='string')?window.sap_s_account:'sap-blackhole';

/* Get page/server info */
var sap_s = new Object();
sap_s.lastFileUpdate = 'DTM:2019.01.17'; // ACR.josh
sap_s.siteCode = 'scn-blog';
sap_s.dtmPropertyName = 'SAP Blogs';
sap_s.linkInternalFilters = 'javascript:,activities.sap.com,answers.sap.com,archive.sap.com,blogs.sap.com,messages.sap.com,moderators.sap.com,people.sap.com';
sap_s.thisHost=(typeof(s_devHost)!='undefined')?s_devHost:window.location.host.toLowerCase();
sap_s.thisPathName=(typeof(s_devPath)!='undefined')?s_devPath:window.location.pathname.toLowerCase();
sap_s.thisSearch=(typeof(s_devSearch)!='undefined')?s_devSearch:window.location.search.toLowerCase();
sap_s.thisProtocol=window.location.protocol.toLowerCase();
try {
    sap_s.thisParentPath=(typeof(s_devParentPath)!='undefined')?s_devParentPath:(typeof(window.top.location)!='undefined' && window.top.location!=window.location)?window.top.location.pathname:false;
} catch(e) {
    sap_s.thisParentPath = false;
}

/* DAL */
sap_s.dynamicAccountSelection = true;
sap_s.dynamicAccountList = 'sapglobal=blogs.sap.com';


sap_s.setDemandBaseVars=function() {
  var s=this;
    /* new demandbase integration */
    try {
   
      s._dtm_db = _satellite.getVar('demandbase');
      if (s._dtm_db&&s._dtm_db.data) {
        console.log('DTM: demandbase data: ',s._dtm_db.data);

        s.new_db={
          _delim:':',
          _nonOrgMatchLabel:'[n/a]',
          _contextData: {
            /* eVar63 */
            s_dmdbase:[ 
              {"id":"demandbase_sid",    "max_size":10},
              {"id":"company_name",      "max_size":40},
              {"id":"industry",          "max_size":40},
              {"id":"sub_industry",      "max_size":20},
              {"id":"employee_range",    "max_size":30},
              {"id":"revenue_range",     "max_size":20},
              {"id":"audience",          "max_size":30},
              {"id":"audience_segment",  "max_size":30},
              {"id":"marketing_alias",   "max_size":25}
            ],
            /* eVar64 */
            s_dmdbase_custom:[
              {"id": "city",                                 "max_size":40},
              {"id": "state",                                "max_size":3},
              {"id": "zip",                                  "max_size":10},
              {"id": "country",                              "max_size":5},
              {"id": "b2b",                                  "max_size":5},
              {"id": "b2c",                                  "max_size":5},
              {"id": ["watch_list","BP_ID"],                 "max_size":30}, 
              {"id": ["watch_list","account_status"],        "max_size":45}, 
              {"id": ["watch_list","account_focus"],         "max_size":38},
              {"id": ["watch_list","hybris_account_status"], "max_size":45}, 
              {"id": ["watch_list","hybris_account_focus"],  "max_size":18}
            ],
            /* eVar65 */
            s_dmdbase_downstream:[
              { 'id': 'fortune_1000',    'max_size': 5 },
              { 'id': 'forbes_2000',     'max_size': 5 },
              { 'id': 'primary_sic',     'max_size': 8 },
              { 'id': 'employee_count',  'max_size': 7 },
              { 'id': 'web_site',        'max_size': 30 },
              { 'id': 'stock_ticker',    'max_size': 6 },
              { 'id': 'traffic',         'max_size': 30 },
              { 'id': 'information_level','max_size': 10 }
            ]
          }
        }  
        var p,v;
        var cd = s.new_db._contextData;
        for (var c in cd) {
          if (cd.hasOwnProperty(c)) {
            s.contextData[c]=[];
            for (var i=0,l=cd[c].length;i<l;i++) {
              v='';
              p = cd[c][i];
              if ( 
                  (p.id instanceof Array)
                   &&
                  (typeof s._dtm_db.data[p.id[0]]!='undefined')
                   &&
                  (typeof s._dtm_db.data[p.id[0]][p.id[1]]!='undefined')
                ){
                  v=s._dtm_db.data[p.id[0]][p.id[1]];
              }
              else if (
                  (typeof s._dtm_db.data[p.id]!='undefined')
                ){
                  v=s._dtm_db.data[p.id];
              }  
              v=((''+v)||s.new_db._nonOrgMatchLabel).slice(0,p.max_size);
              if (v=='null') v=s.new_db._nonOrgMatchLabel;
              var rep = new RegExp(s.new_db._delim,'g');
              v=v.replace(rep,'.').replace(/^\s+|\s+$/g,''); 
              s.contextData[c].push(v);  
            } // end for
          } // end if
          s.contextData[c]=s.contextData[c].join(s.new_db._delim);
        }

      } // end if s._dtm_db&&s._dtm_db.data

      
    } catch (e) {
      console.log(e);
    }
 
} // end s.setDemandBaseVars

// Local download handler filter
function s_checkLocalDownloadHandler(url) {
    return url;
}

// local callback for v35 logic
function s_checkLocalInternalFilter() {
  var refDomain = document.referrer.split('/');
  if (refDomain&&typeof refDomain[2]!='undefined')
	  refDomain = refDomain[2].toLowerCase();
  else
	  refDomain = '';
  /* standard lif stuff */
  if (sap_s.linkInternalFilters) {
    var d = sap_s.linkInternalFilters.split(',');
    for (var c=0;c<d.length;c++) {
      if ( refDomain.indexOf(d[c].toLowerCase())>-1 ) 
        return true;
    }
  }
}

/** trackActivity placeholder **/
sap_s.trackActivity = function(data) {
  var s = this;
	var data = data||{};
	var p = {};

	s.consoleLog('trackActivity: data: ',data);
  
	var actionType = (''+(data.actionType||'')).toUpperCase();
	var objectType = (''+(data.objectType||'')).toLowerCase();
	var contentURL = data.contentURL||'';
	
	p.events = 'event23=1';
  p.products = '';
	p.eVar44 = [actionType,objectType].join('|');
	p.prop44 = 'D=v44';
	p.eVar45 = '+1';
	p.prop45 = 'D=g';
	
	s.trackData(p,'o');
}	// end trackActivity	

function local_sap_s() {

/** stuff to mimic omni.epx **/
  // set client cookie if none exists
  if (!sap_s.c_rr('client')) {
    var GUID = sap_s.UUID.genV1().toString();
    sap_s.expDate=new Date();
    sap_s.expDate.setDate(sap_s.expDate.getDate()+720);
    sap_s.c_wr('client',GUID,sap_s.expDate);
  }
  window.omni_value = sap_s.c_rr('client');
  // set SAP.TTC cookie if none exists
  if (!sap_s.c_rr('SAP.TTC')) {
    sap_s.s_ttc = s_getCurrentUnixTimeStamp();
    window.omni_ttc = sap_s.s_ttc;
    sap_s.expDate=new Date();
    sap_s.expDate.setDate(sap_s.expDate.getDate()+90);
    sap_s.c_wr('SAP.TTC',sap_s.s_ttc,sap_s.expDate);
  }	
  // omni_is1stviewinsession
  if (!sap_s.c_rr('session')) {
    var GUID = sap_s.UUID.genV1().toString();
	  sap_s.c_wr('session',GUID);
		window.omni_is1stviewinsession = 1;
	}

    sap_s.server = sap_s.siteCode;
    sap_s.prop2 = 'us-en';
    sap_s.prop5 = 'glo';
    sap_s.linkDownloadFileTypes = 'csv,exe,zip,wav,mp3,mov,mpg,avi,wmv,pdf,doc,docx,xls,xlsx,ppt,pptx,rar,sda';

    /* page name */
    if (!sap_s.pageName || sap_s.pageName == '') { 
      sap_s.pageName = (sap_s.server + ':' + sap_s.thisPathName).toLowerCase().substring(0, 254);
    }    

    /* channel */
    if (!sap_s.channel) 
      sap_s.channel = sap_s.thisPathName.replace(/^\//,'').split('/')[0].toLowerCase();			
    if (sap_s.channel) sap_s.channel.replace(/\.html$/,'');
    if (!sap_s.channel) sap_s.channel='no site section';

				
    // get error page url if 404 error page
    if (sap_s.pageType) 
    {
        if (!sap_s.prop27) sap_s.prop27 = location.href;
        if (!sap_s.pageName) sap_s.pageName = sap_s.server + ":errorpage";
    }

    /* login/member stuff */
    if (!sap_s.prop9)
      sap_s.prop9 = 'logN';

    if (sap_s.prop10) {
      sap_s.eVar10 = sap_s.prop10;
      sap_s.prop9 = 'logY';
    }
		
    if( String(sap_s.getPreviousValue(sap_s.prop9,'c9')).match(/^(no value|logN|)$/i) && sap_s.prop9 == 'logY') 
      sap_s.events=sap_s.apl(sap_s.events,'event8',',',2);		

  // set demandbase vars
  sap_s.setDemandBaseVars();
			
		
} // end local_s


/**** temp GUID generator for session and client cookies ****/
    /**
    * UUID.js: The RFC-compliant UUID generator for JavaScript.
    *
    * @fileOverview
    * @author LiosK
    * @version 3.2
    * @license The MIT License: Copyright (c) 2010-2012 LiosK.
    * @url https://github.com/LiosK/UUID.js
    */
    /** @constructor */
    sap_s.UUID = (function(overwrittenUUID) {
    function UUID() {}
    UUID.generate = function() {
      var rand = UUID._getRandomInt, hex = UUID._hexAligner;
      return hex(rand(32), 8) // time_low
            + "-"
            + hex(rand(16), 4) // time_mid
            + "-"
            + hex(0x4000 | rand(12), 4) // time_hi_and_version
            + "-"
            + hex(0x8000 | rand(14), 4) // clock_seq_hi_and_reserved clock_seq_low
            + "-"
            + hex(rand(48), 12); // node
    };
    UUID._getRandomInt = function(x) {
      if (x < 0) return NaN;
      if (x <= 30) return (0 | Math.random() * (1 << x));
      if (x <= 53) return (0 | Math.random() * (1 << 30))
                        + (0 | Math.random() * (1 << x - 30)) * (1 << 30);
      return NaN;
    };
    UUID._getIntAligner = function(radix) {
      return function(num, length) {
        var str = num.toString(radix), i = length - str.length, z = "0";
        for (; i > 0; i >>>= 1, z += z) { if (i & 1) { str = z + str; } }
        return str;
      };
    };
    UUID._hexAligner = UUID._getIntAligner(16);
    UUID.FIELD_NAMES = ["timeLow", "timeMid", "timeHiAndVersion",
                        "clockSeqHiAndReserved", "clockSeqLow", "node"];
    UUID.FIELD_SIZES = [32, 16, 16, 8, 8, 48];
    UUID.genV4 = function() {
      var rand = UUID._getRandomInt;
      return new UUID()._init(rand(32), rand(16), // time_low time_mid
                              0x4000 | rand(12), // time_hi_and_version
                              0x80 | rand(6), // clock_seq_hi_and_reserved
                              rand(8), rand(48)); // clock_seq_low node
    };
    UUID.parse = function(strId) {
      var r, p = /^\s*(urn:uuid:|\{)?([0-9a-f]{8})-([0-9a-f]{4})-([0-9a-f]{4})-([0-9a-f]{2})([0-9a-f]{2})-([0-9a-f]{12})(\})?\s*$/i;
      if (r = p.exec(strId)) {
        var l = r[1] || "", t = r[8] || "";
        if (((l + t) === "") ||
            (l === "{" && t === "}") ||
            (l.toLowerCase() === "urn:uuid:" && t === "")) {
          return new UUID()._init(parseInt(r[2], 16), parseInt(r[3], 16),
                                  parseInt(r[4], 16), parseInt(r[5], 16),
                                  parseInt(r[6], 16), parseInt(r[7], 16));
        }
      }
      return null;
    };
    UUID.prototype._init = function() {
      var names = UUID.FIELD_NAMES, sizes = UUID.FIELD_SIZES;
      var bin = UUID._binAligner, hex = UUID._hexAligner;
      this.intFields = new Array(6);
      this.bitFields = new Array(6);
      this.hexFields = new Array(6);
      for (var i = 0; i < 6; i++) {
        var intValue = parseInt(arguments[i] || 0);
        this.intFields[i] = this.intFields[names[i]] = intValue;
        this.bitFields[i] = this.bitFields[names[i]] = bin(intValue, sizes[i]);
        this.hexFields[i] = this.hexFields[names[i]] = hex(intValue, sizes[i] / 4);
      }
      this.version = (this.intFields.timeHiAndVersion >> 12) & 0xF;
      this.bitString = this.bitFields.join("");
      this.hexString = this.hexFields[0] + "-" + this.hexFields[1] + "-" + this.hexFields[2]
                     + "-" + this.hexFields[3] + this.hexFields[4] + "-" + this.hexFields[5];
      this.urn = "urn:uuid:" + this.hexString;
    
      return this;
    };
    UUID._binAligner = UUID._getIntAligner(2);
    UUID.prototype.toString = function() { return this.hexString; };
    UUID.prototype.equals = function(uuid) {
      if (!(uuid instanceof UUID)) { return false; }
      for (var i = 0; i < 6; i++) {
        if (this.intFields[i] !== uuid.intFields[i]) { return false; }
      }
      return true;
    };
    UUID.genV1 = function() {
      var now = new Date().getTime(), st = UUID._state;
      if (now != st.timestamp) {
        if (now < st.timestamp) { st.sequence++; }
        st.timestamp = now;
        st.tick = UUID._getRandomInt(4);
      } else if (Math.random() < UUID._tsRatio && st.tick < 9984) {
        st.tick += 1 + UUID._getRandomInt(4);
      } else {
        st.sequence++;
      }
      var tf = UUID._getTimeFieldValues(st.timestamp);
      var tl = tf.low + st.tick;
      var thav = (tf.hi & 0xFFF) | 0x1000; // set version '0001'
      st.sequence &= 0x3FFF;
      var cshar = (st.sequence >>> 8) | 0x80; // set variant '10'
      var csl = st.sequence & 0xFF;
      return new UUID()._init(tl, tf.mid, thav, cshar, csl, st.node);
    };
    UUID.resetState = function() {
      UUID._state = new UUID._state.constructor();
    };
    UUID._tsRatio = 1 / 4;
    UUID._state = new function UUIDState() {
      var rand = UUID._getRandomInt;
      this.timestamp = 0;
      this.sequence = rand(14);
      this.node = (rand(8) | 1) * 0x10000000000 + rand(40); // set multicast bit '1'
      this.tick = rand(4); // timestamp fraction smaller than a millisecond
    };
    UUID._getTimeFieldValues = function(time) {
      var ts = time - Date.UTC(1582, 9, 15);
      var hm = ((ts / 0x100000000) * 10000) & 0xFFFFFFF;
      return { low: ((ts & 0xFFFFFFF) * 10000) % 0x100000000,
                mid: hm & 0xFFFF, hi: hm >>> 16, timestamp: ts };
    };
    UUID.makeBackwardCompatible = function() {
      var f = UUID.generate;
      UUID.generate = function(o) {
        return (o && o.version == 1) ? UUID.genV1().hexString : f.call(UUID);
      };
      UUID.makeBackwardCompatible = function() {};
    };
    UUID.overwrittenUUID = overwrittenUUID;
    return UUID;
    })(sap_s.UUID);

/*** EOF ***/