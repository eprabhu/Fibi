import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { HttpClient } from '@angular/common/http';
import { HTTP_ERROR_STATUS } from '../app-constants';
import { Observable, of } from 'rxjs';
import { catchError } from 'rxjs/operators';

@Injectable()
export class ProjectOverviewService {

    isEditParentComment = false;
    editParentCommentId = null;

    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    getCOIProjectOverviewDetails(projectOverviewRequestObject: any): Observable<any> {
        const url = `${this._commonService.baseUrl}/project/fetchDashbaord`;
        return this._http.post<any>(url, projectOverviewRequestObject).pipe(
            catchError((err) => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching COI Project overview List failed. Please try again.');
                return of({ projects: [], updatedTimestamp: null });
        }));
    }

    addProjectOverviewComment(params: any) {
        return this._http.post(this._commonService.baseUrl + '/project/saveComment', params, { responseType: 'text' });
    }

    getProjectOverviewComments(params) {
        return this._http.post(this._commonService.baseUrl + '/project/fetchComment', params);
    }

    updateProjectOverviewComment(params: any) {
        return this._http.patch(this._commonService.baseUrl + '/project/updateComment', params, { responseType: 'text' });
    }

    deleteProjectOverviewComments(CommentId) {
        return this._http.delete(`${this._commonService.baseUrl}/project/deleteComment/${CommentId}`, { responseType: 'text' });
    }

}   
