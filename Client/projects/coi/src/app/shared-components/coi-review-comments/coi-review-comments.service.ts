import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiReviewCommentsService {

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    loadCoiReviewComments(params: any) {
        return this._http.post(this._commonService.baseUrl + '/loadCoiReviewComments', params);
    }

    deleteReviewComment(coiReviewCommentId: any) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteCOIReviewComment/${coiReviewCommentId}`);
    }

    downloadCoiReviewAttachment(attachmentId: number) {
        return this._http.get(`${this._commonService.baseUrl}/downloadCoiReviewAttachment`, {
            headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
            responseType: 'blob'
        });
    }

    deleteReviewAttachment(attachmentId: number) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteReviewAttachment/${attachmentId}`);
    }

}
