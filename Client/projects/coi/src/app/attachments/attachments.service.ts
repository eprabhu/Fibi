import { HttpClient, HttpHeaders} from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class AttachmentsService {

    constructor(private _http: HttpClient,
        private _commonService: CommonService) { }

    fetchAllAttachmentsForPerson(personId) {
        return this._http.get(`${this._commonService.baseUrl}/loadAllAttachmentsForPerson/${personId}`);
    }

    deleteAttachment(attachmentNumber: number) {
        return this._http.post(`${this._commonService.baseUrl}/deleteAttachment`, {attachmentNumber}, { responseType: 'text' });
    }

    downloadAwardAttachment(attachmentId) {
        return this._http.get(`${this._commonService.baseUrl}/downloadAttachment`, {
            headers: new HttpHeaders().set('attachmentId', attachmentId.toString()),
            responseType: 'blob'
        });
    }

}
