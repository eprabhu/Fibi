import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class ReviewerDashboardService {

    reviewerRequestObject = new ReviewerDashboardRequest();
    isAdvanceSearch: any;
    searchDefaultValues: NameObject = new NameObject();
    sortCountObject: SortCountObj = new SortCountObj();
    sort: any;

    constructor(private _http: HttpClient,
        private _commonService: CommonService) { }


    getCOIReviewerDashboard(params: any) {
        return this._http.post(this._commonService.baseUrl + '/getCOIReviewerDashboard', params).pipe(catchError((err) => {
            return of();
        }));
    }
}

export class ReviewerDashboardRequest {
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
    pageNumber = 20;
    sort: any = { 'updateTimeStamp': 'desc' };
    tabName = '';
    advancedSearch = 'L';
    currentPage = 1;
    constructor(tabname?) {
        this.tabName = tabname ? tabname : 'NEW_SUBMISSIONS';
    }
}

export class SortCountObj {
    coiDisclosureNumber = 0;
    disclosurePersonFullName = 0;
    disclosureCategoryType = 0;
    disclosureStatus = 0;
    expirationDate = 0;
    certificationDate = 0;
    certifiedAt = 0;
    updateTimeStamp = 2;
}

export class NameObject {
    entityName = '';
    personName = '';
    departmentName = '';
}
