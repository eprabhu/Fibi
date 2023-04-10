import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { slowSlideInOut} from '../../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from '../entity-management.service';

@Component({
  selector: 'app-view-entity-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  animations: [slowSlideInOut]
})
export class ViewEntityDetailsComponent implements OnInit {

  entityDetails:any = {};
  constructor(private _router:Router,private _route:ActivatedRoute,private _entityManagementService:EntityManagementService) { }

  ngOnInit() {
    const entityManageId = this._route.snapshot.queryParamMap.get('entityManageId');
    this.getEntityDetails(entityManageId);


  }

  backToList(){
    // this.hideEntityDetails.emit(false);
    this._router.navigate(['/coi/entity-management'])
  }

  modifyEntity() {
    this._router.navigate(['/coi/create-disclosure/entity-details'])
  }

  getEntityDetails(entityId){
    this._entityManagementService.getEntityDetails(entityId).subscribe((res:any)=>{
      this.entityDetails = res.coiEntity;
      // debugger
    })
  }
}
