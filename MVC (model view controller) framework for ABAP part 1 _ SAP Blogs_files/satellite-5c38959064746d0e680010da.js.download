_satellite.pushBlockingScript(function(event, target, $variables){
  handleQueryAjaxActions([
    [hasParams({action: 'like_a_post', actionLike: 'doLike'}), sentActivity('like', 'blogpost')],
    [hasParams({action: 'like_a_comment', actionLike: 'doLike'}), sentActivity('like', 'comment')],
    [hasParams({action: 'like_a_post', actionLike: 'doUnLike'}), sentActivity('cancel-like', 'blogpost')],
    [hasParams({action: 'like_a_comment', actionLike: 'doUnLike'}), sentActivity('cancel-like', 'comment')],
    [hasAction('sap-update-comment'), sentActivity('update', 'comment')],
    [hasAction('sap-delete-comment'), sentActivity('delete', 'comment')], //delete from blogpost page
    [hasParams({action: 'delete-comment', trash: '1'}), sentActivity('delete', 'comment')], //delete from admin page
    [hasParams({action: 'delete-comment', untrash: '1'}), sentActivity('publish', 'comment')], //undelete from admin page
    [hasAction('wprc_add_report'), sentActivity('publish', 'alert')],
    [hasAction('report_agree_post'), sentActivity('confirm', 'alert')],
    [hasAction('report_disagree_post'), sentActivity('reject', 'alert')]]
);
handleFetchActions([
    [hasParams({last_status: 'follow', action: 'ss_change_follow_status',content_type:'blogpost'}), sentActivity('follow', 'blogpost')],
    [hasParams({last_status: 'follow', action: 'ss_change_follow_status', content_type:'sap_tag'}), sentActivity('follow', 'tag')],
    [hasParams({last_status: 'not-follow', action: 'ss_change_follow_status', content_type:'blogpost'}), sentActivity('stop-following', 'blogpost')],
    [hasParams({last_status: 'not-follow', action: 'ss_change_follow_status', content_type:'sap_tag'}), sentActivity('stop-following', 'tag')]]
);

/**
 * Handles all $.ajax(...) calls.
 * @param {Array.<[requestPredicate,actionCallback]>} handlers.
 */
function handleQueryAjaxActions(handlers) {
    var getUrlParams = function (url) {
        return url.split('&')
            .reduce(function (obj, paramStr) {
                var paramsArr = paramStr.split('=');
                obj[paramsArr[0]] = paramsArr[1];
                return obj;
            }, {});
    };

    jQuery(document).ajaxSuccess(function (event, xhr, ajaxOptions, data) {
        if (ajaxOptions.data) {
            var urlParams = getUrlParams(ajaxOptions.data);
            for (var i = 0; i < handlers.length; i++) {
                var predicate = handlers[i][0];
                var callback = handlers[i][1];
                if (predicate(urlParams)) {
                    callback();
                    return;
                }
            }
        }
    });
}

/**
 * Handles all Fetch API calls.
 * @param {Array.<[requestPredicate,actionCallback]>} handlers.
 */
function handleFetchActions(handlers) {
    var originalFetch = fetch;
    fetch = function () {
        var args = arguments;
        return originalFetch.apply(this, arguments).then(function (data) {
            if (args && args[1] && args[1].body) {
                var resp = data.clone();
                resp.json().then(function (respData) {
                    var resultParams = Array.from(args[1].body.entries()).reduce(function (obj, entry) {
                        obj[entry[0]] = entry[1];
                        return obj;
                    }, respData);
                    for (var i = 0; i < handlers.length; i++) {
                        var predicate = handlers[i][0];
                        var callback = handlers[i][1];
                        if (predicate(resultParams)) {
                            callback();
                            return resp;
                        }
                    }
                });
            }
            return data;
        });
    };
}

/**
 * @callback requestPredicate checks whether request satisfy conditions
 * @param {Object} request parameters
 * @return {boolean}
 */
/**
 * @param {string} action that should be in the request
 * @return {requestPredicate} that checks whether request has passed action
 */
function hasAction(action) {
    return function (urlParams) {
        return (typeof urlParams['action'] !== 'undefined') && urlParams['action'] === action;
    }
}

/**
 * @param {Object} desiredParams
 * @return {requestPredicate}
 */
function hasParams(desiredParams) {
    return function (urlParams) {
        for (var desiredName in desiredParams) {
            if (!urlParams[desiredName] || urlParams[desiredName] !== desiredParams[desiredName]) {
                return false;
            }
        }
        return true;
    }
}

/**
 * @callback actionCallback fire when action occure
 */

/**
 * @param  {String} actionType
 * @param {String} objectType
 * @return {actionCallback} that sends activity with the params above for tracking
 */
function sentActivity(actionType, objectType) {
    return function () {
        var activityParams = {"objectType": objectType, "actionType": actionType};
        var location = window.location;
        activityParams.contentURL = location.host + location.pathname;
        sap_s.trackActivity(activityParams);
    };
}
});
