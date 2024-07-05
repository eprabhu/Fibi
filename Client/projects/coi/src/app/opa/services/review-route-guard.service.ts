import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { OPA_REDIRECT_URL } from '../../app-constants';

@Injectable()
export class ReviewRouteGuardService {

  constructor(private _commonService: CommonService,private _router: Router) { }

  canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): any {
    if(this._commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE'])) {
        return true;
    } else {
        this._router.navigate([OPA_REDIRECT_URL], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
    }
  }

}
