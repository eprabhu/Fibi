import {Injectable} from '@angular/core';
import {CommonService} from "../../common/services/common.service";
import {HttpClient} from "@angular/common/http";
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Injectable()
export class UserDisclosureService {

    constructor(private _commonService: CommonService, private _http: HttpClient) {
    }

    getCOIDashboard(param) {
        return this._http.post(this._commonService.baseUrl + '/getCOIDashboard', param).pipe(catchError((err) => {
              this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching disclosure list failed. Please try again.');
              return of();
          }));
    }

    getCOIDashboardCount(param) {
        return this._http.post(this._commonService.baseUrl + '/getTabCount', {
            "advancedSearch": "L",
            "pageNumber": 2,
            "sort": {
                "createTimestamp": "asc"
            },
            "filterType": "ALL",
            "isDownload": false
        });
    }

    getDisclosureHistory(param) {
        return this._http.post(this._commonService.baseUrl + '/fcoiDisclosure/historyDashboard',param);
    }

    createOPA(personId, homeUnit) {
        return this._http.post(this._commonService.opaUrl + '/createOPA', {personId, homeUnit});
    }
}
