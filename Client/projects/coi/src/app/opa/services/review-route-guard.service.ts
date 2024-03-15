import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Router, RouterStateSnapshot } from '@angular/router';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class ReviewRouteGuardService {

  constructor(private _commonService: CommonService,private _router: Router) { }

  canActivate(route: ActivatedRouteSnapshot, _state: RouterStateSnapshot): any {
    if(this._commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE'])) {
        return true;
    } else {
        this._router.navigate(['/coi/opa/form'], {queryParams: {disclosureId: route.queryParamMap.get('disclosureId')}});
    }
  }

}
