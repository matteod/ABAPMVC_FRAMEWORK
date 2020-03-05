try {
    s_setPercentPageViewedVars();
} catch (e) {
    window.console&&console.error(e);
}

try {
    /** Baindaid for preserving original referrer because of trustarc reload **/
    sap_s._getReferrer = function() {
        var ref = document.cookie.match(/(?:^|;\s*)s_referrer=([^;]*)/);
        if (ref && ref[1]) {
            ref = unescape(ref[1]).split('|');
            if (+ref[0] === 0 && ref[1]) {
                document.cookie = 's_referrer=' + escape('1|' + ref[1]) + ';path=/;domain=' + location.hostname.split('.').slice(-2).join('.');
                return ref[1];
            }
        }
        return document.referrer || '';
    }
    sap_s.referrer = sap_s._getReferrer();
} catch (e) {
    window.console&&console.error(e);
}

try {
    sap_s.t();
} catch (e) {
    window.console&&console.error(e);
}