import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class AttachmentsService {

    constructor(private _http: HttpClient,
        private _commonService: CommonService) { }

    fetchAllAttachmentsForPerson(personId) {
        return this._http.get(`${this._commonService.baseUrl}/loadAllAttachmentsForPerson/${personId}`);
    }

}
