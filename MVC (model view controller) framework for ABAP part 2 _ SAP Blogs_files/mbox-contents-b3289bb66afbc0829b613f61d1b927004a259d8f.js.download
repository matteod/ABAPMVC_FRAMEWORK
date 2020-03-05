try {
    /**
      demandbase integration 1/2
      note: 2/2 is located in data element "demandbase"
     **/
    var demandbaseParse = function(dbInfo) {
        try {
            if (!dbInfo) return '';
            window._demandbaseInfo = dbInfo;
            // set watch_list default if not returned from DB
            if (typeof dbInfo.watch_list === 'undefined') {
                window._demandbaseInfo.watch_list = {
                    'account_status': '[n/a]'
                };
            }
        } catch (e) {
            window.console && console.error(e);
        }
    }
    var dbVal = _satellite.getVar('demandbase');
    var src = '//api.demandbase.com/api/v2/ip.json?key=' + dbVal.key + '&var=dbInfo&callback=' + dbVal.callback + '&query=' + dbVal.query;
    document.write(unescape("%3Cscript src='" + document.location.protocol + src + "' type='text/javascript'%3E%3C/script%3E"));
} catch (e) {
    window.console && console.error(e);
}