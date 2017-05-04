export const REQUEST_POSTS = "REQUEST_POSTS";
export const RECEIVE_POST = "RECEIVE_POST";

import { postsPromise, contentPromise } from "./github"

export function requestPosts() {
    return {
	type: REQUEST_POSTS,
    }
}

export function receivePost(title, date, content) {
    return {
	type: RECEIVE_POST,
	title,
	date,
	content,
    }
}

function getPost(dispatch, data) {
    const content = atob(data.content)
    dispatch(receivePost(
	"Title",
	"2017-02-01",
	content,
    ))
}

function getAllPosts(dispatch, data) {
    data.forEach((file) => {
	contentPromise(file.path)
	    .then(response => response.data)
	    .then(data => getPost(dispatch, data))
    })
}

export function fetchPosts() {
    return (dispatch) => {
	dispatch(requestPosts());

	return postsPromise
	    .then(response => response.data)
	    .then(
		data => getAllPosts(dispatch, data)
            )
    }
}
