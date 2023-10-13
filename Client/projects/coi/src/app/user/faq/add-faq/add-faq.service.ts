import { Injectable } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class AddFaqService {
    newlyAddedData: any = [];
    constructor(private _commonService: CommonService, private _http: HttpClient) {}

    getCategory(params) {
        return this._http.post(this._commonService.baseUrl + '/listFaqCategory', params );
    }
    saveFaq(params) {
        return this._http.post(this._commonService.baseUrl + '/addFaqAttachment', params );
    }
    savequestion(params) {
        return this._http.post(this._commonService.baseUrl + '/saveFaq', params );
    }
}
