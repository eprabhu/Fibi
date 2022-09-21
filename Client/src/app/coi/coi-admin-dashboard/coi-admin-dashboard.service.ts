import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';

@Injectable()
export class CoiAdminDashboardService {

    coiRequestObject = new CoiDashboardRequest();

    constructor(private _http: HttpClient,
                private _commonService: CommonService) {
    }

    getCOIAdminDashboard(params: any) {
        return this._http.post(this._commonService.baseUrl + '/getCOIAdminDashboard', params);
    }

    loadDisclosureAdminDashboardCounts() {
        return this._http.get(this._commonService.baseUrl + '/loadDisclosureAdminDashboardCounts');
    }

    startCOIReview(coiReviewId: number) {
        return this._http.post(`${this._commonService.baseUrl}/startCOIReview`,
            {coiReview: {coiReviewId}});
    }

    completeCOIReview(coiReviewId: number) {
        return this._http.post(`${this._commonService.baseUrl}/completeCOIReview`,
            {coiReview: {coiReviewId}});
    }

    loadCoiReviewComments(req: any) {
        return this._http.post(this._commonService.baseUrl + '/loadCoiReviewComments', req);
    }

    addCOIReviewComment(params: any) {
        const formData = new FormData();
        formData.append('formDataJson', JSON.stringify(params));
        return this._http.post(this._commonService.baseUrl + '/addCOIReviewComment', formData);
    }
}

export class CoiDashboardRequest {
    isDownload = false;
    property1 = '';
    property2 = null;
    property3 = null;
    property4 = null;
    property5 = null;
    property6 = null;
    property7 = null;
    property8 = null;
    property9 = null;
    property10 = null;
    property11 = null;
    property12 = null;
    property13 = null;
    property14 = null;
    property15 = false;
    pageNumber = 20;
    sort: any = {};
    tabName = 'ALL_DISCLOSURES';
    advancedSearch = 'L';
    currentPage = 1;
}
