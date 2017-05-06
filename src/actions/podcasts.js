import { podcastsPromise } from './github';

export const REQUEST_PODCASTS = "REQUEST_PODCASTS";
export const RECEIVE_PODCASTS = "RECEIVE_PODCASTS";

function requestPodcasts() {
    return {
        type: REQUEST_PODCASTS,
    }
}

function receivePodcasts(content) {
    return {
        type: RECEIVE_PODCASTS,
        content,
    }
}

export function fetchPodcasts(dispatch) {
    return (dispatch) => {
        dispatch(requestPodcasts());

        return podcastsPromise
            .then(response => response.data.content)
            .then(base64 => atob(base64))
            .then(data => JSON.parse(data))
            .then(json => {
                console.log(json)
                return json.podcasts})
            .then(podcasts => dispatch(receivePodcasts(podcasts)))
    }
}
