import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';

@Injectable()
export class EntityDetailsGuardService implements CanActivate {

  constructor(private _entityDetails: EntityDetailsService) { }

  canActivate(route: ActivatedRouteSnapshot): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const ENTITY_ID = route.queryParamMap.get('entityId');
      this._entityDetails.getSFIDetails(ENTITY_ID).subscribe((res: any) => {
        this._entityDetails.$entityDetails.next(res);
        resolve(true);
      })
    });
  }
}
