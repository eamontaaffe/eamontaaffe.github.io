import {
    REQUEST_PODCASTS,
    RECEIVE_PODCASTS,
} from '../actions';

const initialPodcasts = {
    isFetching: false,
    content: [],
}

function podcasts(state=initialPodcasts, action) {
    switch(action.type) {
    case REQUEST_PODCASTS:
        return {
            ...state,
            isFetching: true,
        }
    case RECEIVE_PODCASTS:
        return {
            ...state,
            isFetching: false,
            content: action.content,
        }
    default:
        return state;
    }
    
}

export default podcasts;
