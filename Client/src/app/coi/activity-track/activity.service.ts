import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class ActivityService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    addCOIReviewComment(params: any, uploadedFiles: any) {
        const formData = new FormData();
        if (uploadedFiles) {
            for (const file of uploadedFiles) {
                formData.append('files', file, file.fileName);
            }
        }
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(this._commonService.baseUrl + '/addCOIReviewComment', formData);
    }

}
