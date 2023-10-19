import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

import { CommonService } from '../../common/services/common.service';

@Injectable()
export class ReviewService {

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    saveOrUpdateCoiReview(params: any) {
        return this._http.post(this._commonService.opaUrl + '/review', params);
    }

    getCoiReview(disclosureId: number) {
        return this._http.get(`${this._commonService.opaUrl}/review/${disclosureId}`);
    }


    deleteReview(coiReviewId: any) {
        return this._http.delete(`${this._commonService.opaUrl}/review/${coiReviewId}`);
    }

    reviewHistory(disclosureId: any) {
        return this._http.get(`${this._commonService.baseUrl}/reviewHistory/${disclosureId}`);
    }

}
