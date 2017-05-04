import {
    REQUEST_ABOUT,
    RECEIVE_ABOUT,
} from '../actions';

const initialAbout = {
    isFetching: false,
    content: "",
}

function about(state = initialAbout, action) {
    switch (action.type) {
    case REQUEST_ABOUT:
	return {
	    ...state,
	    isFetching: true
	}
    case RECEIVE_ABOUT:
	return {
	    ...state,
	    isFetching: false,
	    content: action.content,
	}
    default:
	return state
    }
}

export default about;
