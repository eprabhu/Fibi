import {Injectable} from '@angular/core';
import {CommonService} from '../../common/services/common.service';
import {HttpClient, HttpHeaders} from '@angular/common/http';

@Injectable()
export class ReviewCommentsService {


    isEditParentComment = false;
    editParentCommentId = null;

    constructor(private _commonService: CommonService, private _http: HttpClient) {
    }

    getSectionsTypeCode() {
        return this._http.get(this._commonService.baseUrl + '/getCoiSectionsTypeCode');
    }

    getAdminDetails() {
        return this._http.get(this._commonService.baseUrl + '/adminGroup/adminPersons');
    }

    getCoiReviewComments(params) {
        return this._http.post(this._commonService.baseUrl + '/loadCoiReviewComments', params);

    }

    downloadAttachment(params) {
        return this._http.get(this._commonService.baseUrl + '/attachment/downloadDisclAttachment', {
            headers: new HttpHeaders().set('attachmentId', params.toString()),
            responseType: 'blob'
        });
    }

    deleteCOIAssignee(coiReviewCommentTagId) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteCOIReviewCommentTags/${coiReviewCommentTagId}`);
    }

    deleteCOICommentAttachment(params) {
        return this._http.post(this._commonService.baseUrl + ' /attachment/deleteDisclAttachment', params);
    }

    deleteReviewComments(coiReviewCommentId) {
        return this._http.delete(`${this._commonService.baseUrl}/deleteCOIReviewComment/${coiReviewCommentId}`);
    }

    addCOIReviewComment(params: any, uploadedFiles) {
        const formData = new FormData();
        if (uploadedFiles) {
            uploadedFiles.forEach(file => {
                formData.append('files', file, file.fileName);
            });
        }
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(this._commonService.baseUrl + '/addCOIReviewComment', formData);
    }


}
