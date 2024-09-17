import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { AttachmentSaveRO } from '../../common/services/coi-common.interface';

@Injectable()

export class SharedAttachmentModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }
    
    
    getAttachmentTypes() {
        return this._http.get(this._commonService.baseUrl + '/loadDisclAttachTypes');
    }
    
    saveAttachment(params: { newAttachments: AttachmentSaveRO[] }, uploadedFile: any[]) {
        const formData = new FormData();
        for (const file of uploadedFile) {
            formData.append('files', file, file.name);
        }
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(`${this._commonService.baseUrl}/saveOrReplaceAttachments`, formData);
    }
    
    updateAttachment(attachmentId: number, description: string) {
        return this._http.post(`${this._commonService.baseUrl}/updateAttachmentDetails`, { attachmentId, description });
    }
}
