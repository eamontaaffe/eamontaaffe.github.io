export const REQUEST_POSTS = "REQUEST_POSTS";
export const RECEIVE_POST = "RECEIVE_POST";

import { postsPromise, postPromise, contentPromise } from "./github"

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

export function fetchPosts() {
    return (dispatch) => {
	dispatch(requestPosts());

	return postsPromise
	    .then(response => response.data)
	    .then(
		data => {
		    data.forEach((file) => {
			contentPromise(file.path)
			    .then(response => response.data)
			    .then(data => {
				const content = atob(data.content)
				dispatch(receivePost(
				    "Title",
				    "2017-02-01",
				    content,
				))
			    })
		    })
		})
    }
}
