import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, UrlTree, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { CommonService } from '../common/services/common.service';


@Injectable({
  providedIn: 'root'
})
export class EntityManagementGuardService implements CanActivate{
rightList:string[]=[]


  rights() {
    this._commonService.fetchPermissions().then((right:any)=>{
       this.rightList=right;
      }).catch((error) => {
        console.log('Error fetching permissions:', error);
   })
  }

constructor(private router: Router,
  private _commonService: CommonService) { }
  
canActivate(
): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {

  if (this._commonService.rightList.includes('MANAGEENTITY') || this._commonService.rightList.includes('VIEWENTITY')) {
    return true;
  } else {
       this.router.navigate(['/coi/error-handler/401']);
       return false;
  }
}
}

