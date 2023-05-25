import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { CommonService } from '../common/services/common.service';


@Injectable({
  providedIn: 'root'
})
export class EntityManagementGuardService implements CanActivate{
rightList:string[]=[]


constructor(private router: Router,
  private _commonService: CommonService) { }
  
canActivate(
): boolean{

  if (this._commonService.rightsArray.includes('MANAGE_ENTITY') || this._commonService.rightsArray.includes('VIEW_ENTITY')) {
    return true;
  } else {
       this.router.navigate(['/coi/error-handler/401']);
       return false;
  }
}

}

