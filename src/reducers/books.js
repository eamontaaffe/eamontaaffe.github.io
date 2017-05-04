import {
    REQUEST_BOOKS,
    RECEIVE_BOOKS,
} from '../actions'

const initialBooks = {
    isFetching: false,
    content: [],
}

function books(state=initialBooks, action) {
    switch(action.type) {
    case REQUEST_BOOKS:
        return {
            ...state,
            isFetching: true,
        }
    case RECEIVE_BOOKS:
        return {
            ...state,
            isFetch: false,
            content: action.content,
        }
    default:
        return state
    }
}

export default books;
