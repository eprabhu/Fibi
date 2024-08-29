import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';

@Injectable({
    providedIn: 'root'
})
export class EntityAttachmentModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getAttachmentDetails(sectionCode , entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/attachment/getAttachmentsBySectionCode/${sectionCode}/${entityId}`);
    }

    // SaveAttachments(saveAttachmentRO){
    //     return this._http.post(this._commonService.baseUrl + '/entity/attachment/saveFile', saveAttachmentRO);
    // }

    SaveAttachment(sectionCode, newAttachments, uploadedFile) {
        const formData = new FormData();
        for (const file of uploadedFile) {
          formData.append('files', file);
        }
        formData.append('formDataJson', JSON.stringify({
          'newAttachments': newAttachments,
          'sectionCode': sectionCode
        }));
        return this._http.post(this._commonService.baseUrl + '/entity/attachment/saveFile', formData);
    }

    deleteAttachment() {
        return this._http.delete(this._commonService.baseUrl + '/entity/attachment/deleteAttachment');
    }

    downloadAwardAttachment( attachmentId ) {
        return this._http.get( this._commonService.baseUrl + '/entity/attachment/downloadAwardAttachment', {
            headers: new HttpHeaders().set( 'attachmentId', attachmentId.toString() ),
            responseType: 'blob'
        } );
    }

}
