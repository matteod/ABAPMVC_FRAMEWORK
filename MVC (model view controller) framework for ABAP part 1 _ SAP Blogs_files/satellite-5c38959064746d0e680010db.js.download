_satellite.pushBlockingScript(function(event, target, $variables){
  var STORAGE_KEY = 'DTM_RELOAD_ACTION';

fillStorageForTrackingActivity();
sendActionsFromSessionStorage();


function fillStorageForTrackingActivity() {
    setUpDeleteBlogpost();
    setUpPublishBlogpost();
    setUpPublishComment();
}

function setUpPublishComment() {
    var $commentForm = jQuery('form#commentform');
    $commentForm.submit(function (event) {
        var isNewComment = $commentForm.find('#new_comment_nonce').length;
        if (isNewComment) {
            saveToSessionStorage('comment', 'publish');
        }
    })
}

function setUpDeleteBlogpost() {
    jQuery('.submitdelete').click(function (event) {
        saveToSessionStorage('blogpost', 'delete');
    });
}

function setUpPublishBlogpost() {
    var $postForm = jQuery('form#post');
    var clickedAction = ''; //to handel wha action initiated form submit. i.e publish, save, needs-more-work..
    $postForm.find('input[value="Publish"][type="Submit"]').click(function () {
        clickedAction = 'publish';
    });
    $postForm.find('input[value="Needs More Work"][type="Submit"]').click(function () {
        clickedAction = 'needs_more_work';
    });
    $postForm.find('input[value="Submit for Review"][type="Submit"]').click(function () {
        clickedAction = 'submit_for_review';
    });
    $postForm.find('input[value="Update"][type="Submit"]').click(function () {
        clickedAction = 'update';
    });
    $postForm.find('input[value="Save"][type="Submit"]').click(function () {
        clickedAction = 'update';
    });

    var tagUpdated = false;
    $postForm.find('#primary-tag').change(function () {
        tagUpdated = true;
    });
    var isNewPost = $postForm.find('#custom-post-status-display').text() === 'Not Saved';

    $postForm.submit(function () {
        var postStatus = $postForm.find('#original_post_status').attr('value');
        var shouldSendTagUpdated =
            tagUpdated && (
                postStatus === 'needs-more-work'
                || postStatus === 'pending'
                || (clickedAction === 'update' && !isNewPost)
            );
        if (clickedAction) {
            saveToSessionStorage('blogpost', clickedAction);
        }
        if (shouldSendTagUpdated) {
            saveToSessionStorage('blogpost', 'update-tag-assignment');
        }
    })
}

/**
 * Saves action to session storage to procede when page reloads
 * @param {String} objectType
 * @param {String} action
 */
function saveToSessionStorage(objectType, action) {
    var storageItem = [];
    var existedItemsStr = sessionStorage.getItem(STORAGE_KEY);
    if (existedItemsStr) {
        storageItem = JSON.parse(existedItemsStr);
    }
    storageItem.push(createActivity(objectType, action));
    sessionStorage.setItem(STORAGE_KEY, JSON.stringify(storageItem));
}

/**
 * Creates activity to send to tracker
 * @param {String} objectType
 * @param {String} action
 * @return {{objectType: String, actionType: String}}
 */
function createActivity(objectType, action) {
    var activityParams = {"objectType": objectType, "actionType": action};
    var location = window.location;
    activityParams.contentURL = location.host + location.pathname;
    return activityParams;
}

/**
 * Get all pre-saved activities from session storage and send them to tracking system.
 */
function sendActionsFromSessionStorage() {
    var activitiesStr = sessionStorage.getItem(STORAGE_KEY);
    if (activitiesStr) {
        var activities = JSON.parse(activitiesStr);
        sessionStorage.removeItem(STORAGE_KEY);

        var location = window.location.host + window.location.pathname;
        activities.forEach(function (activity) {
            activity.contentURL = location;
            sap_s.trackActivity(activity);
        });
    }
}
});
