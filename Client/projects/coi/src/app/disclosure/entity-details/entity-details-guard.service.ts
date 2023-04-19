import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, CanActivate } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';

@Injectable()
export class EntityDetailsGuardService implements CanActivate {

  constructor(private _entityDetailsService: EntityDetailsService) { }

  canActivate(route: ActivatedRouteSnapshot): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const ENTITY_ID = route.queryParamMap.get('entityId');
      this.getSfiLookUp();
      this._entityDetailsService.getSFIDetails(ENTITY_ID).subscribe((res: any) => {
        this._entityDetailsService.$entityDetailsTest.next(res);
      // this.getCoiEntityDetails(ENTITY_ID);
      // this.getCoiEntityRelationships(ENTITY_ID);
      resolve(true);
      })
    });
  }
  getSfiLookUp() {
    this._entityDetailsService.addSFILookUp().subscribe((res: any) => {
      this._entityDetailsService.lookups = res.personEntityRelType;
    });
  }

//    getCoiEntityDetails(ENTITY_ID) {
//     this._entityDetailsService.getCoiEntityDetails(ENTITY_ID).subscribe((res: any) => {
//       this._entityDetailsService.$entityDetails.next(res);
//     })
//   }
  getCoiEntityRelationships(ENTITY_ID) {
    this._entityDetailsService.getRelationshipEntityDetails(ENTITY_ID).subscribe((res: any) => {
      this._entityDetailsService.$relationshipsDetails.next(res);
      console.log('getCoiEntityRelationships', res);

    })
  }
}
