import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../services/common.service';

@Injectable()

export class AddAttachmentModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    addAttachment(params, uploadedFile) {
        const formData = new FormData();
        for (const file of uploadedFile) {
            formData.append('files', file, file.name);
        }
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateAttachments', formData);
    }

    getAttachmentTypes() {
        return this._http.get(this._commonService.baseUrl + '/loadDisclAttachTypes');
    }
}
