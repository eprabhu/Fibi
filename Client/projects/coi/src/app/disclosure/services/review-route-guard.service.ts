import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CoiService } from './coi.service';
import { CommonService } from '../../common/services/common.service';
import { POST_CREATE_DISCLOSURE_ROUTE_URL } from '../../app-constants';

@Injectable()
export class ReviewRouteGuardService {

  constructor(private _commonService: CommonService,private _router: Router,
    ) { }

  canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): any {
    if(this._commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE'])) {
        return true;
    } else {
        this._router.navigate([POST_CREATE_DISCLOSURE_ROUTE_URL], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
    }
  }


}
