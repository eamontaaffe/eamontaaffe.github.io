import {
    REQUEST_POSTS,
    RECEIVE_POST,
} from '../actions';

const initialPosts = {
    isFetching: false,
    content: [],
}

function posts(state=initialPosts, action) {
    switch (action.type) {
    case REQUEST_POSTS:
	return {
	    isFetching: true,
	    content: [],
	};
    case RECEIVE_POST:
	const post = {
	    title: action.title,
	    date: action.date,
	    content: action.content,
            id: action.id,
	}
	return {
	    isFetching: false,
	    content: [
		...state.content,
		post,
	    ],
	};
    default:
	return state;
    }
}

export default posts;
