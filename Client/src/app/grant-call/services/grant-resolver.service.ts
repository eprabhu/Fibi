import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot, Router } from '@angular/router';

import { GrantCallService } from './grant.service';
import { CommonService } from '../../common/services/common.service';
import { catchError } from 'rxjs/operators';
import {of as observableOf,  Observable } from 'rxjs';

@Injectable()
export class GrantCallResolverService implements Resolve<any> {

  constructor( private _grantService: GrantCallService, public _commonService: CommonService,
    private _router: Router) {}

  resolve(route: ActivatedRouteSnapshot) {
    if (route.queryParamMap.get('grantId') == null) {
      return this._grantService.createGrantCall();
    } else {
        return this._grantService.loadGrantById({
          'grantCallId': route.queryParamMap.get('grantId')
        }).pipe(
        catchError(error => {
          console.log('Retrieval error', error);
          if (error.status === 403) {
            this._commonService.forbiddenModule = '15';
            this._router.navigate(['/fibi/error/403']);
            return observableOf(null);
          } else {
            this._router.navigate(['/fibi/dashboard/grantCall']);
            return observableOf(null);
          }
        }));
      }
    }
}
