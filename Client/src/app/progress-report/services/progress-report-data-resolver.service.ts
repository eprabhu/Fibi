/** last updated by Ayush on 28-12-2020 **/

import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { ProgressReportService } from './progress-report.service';
import {catchError} from 'rxjs/operators';
import {of as observableOf} from 'rxjs';

@Injectable()
export class ProgressReportDataResolverService implements Resolve<any> {

    constructor(private _progressReportService: ProgressReportService, public _commonService: CommonService, private _router: Router) { }

    resolve(route: ActivatedRouteSnapshot) {
        if (route.queryParamMap.get('progressReportId')) {
            return this._progressReportService.loadAwardProgressReport(route.queryParamMap.get('progressReportId')).pipe(
                catchError(error => {
                    console.log('Retrieval error', error);
                    if (error.status === 403) {
                        this._commonService.forbiddenModule = '16';
                        this._router.navigate(['/fibi/error/403']);
                        return observableOf(null);
                    } else {
                        this._router.navigate(['/fibi/dashboard/progressReportList']);
                        return observableOf(null);
                    }
                }));
        } else {
            this._router.navigate(['/fibi/dashboard/progressReportList']);
        }
    }

}
