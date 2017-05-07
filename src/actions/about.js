export const REQUEST_ABOUT = "REQUEST_ABOUT";
export const RECEIVE_ABOUT = "RECEIVE_ABOUT";

import { aboutPromise } from "./github"

function requestAbout() {
    return {
	type: REQUEST_ABOUT,
    }
}

function receiveAbout(content) {
    return {
	type: RECEIVE_ABOUT,
	content,
    }
}

export function fetchAbout() {
    return (dispatch) => {
	dispatch(requestAbout());

	return aboutPromise()
	    .then(response => response.data)
	    .then(
		data => {
		    const content = atob(data.content)
		    dispatch(receiveAbout(content))
		}
	    )
    }
}
