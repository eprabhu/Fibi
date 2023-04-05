import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { EntityManagementService } from '../entity-management.service';

@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss']
})
export class EntityListComponent implements OnInit,OnChanges {

  @Output() viewEntityDetails: EventEmitter<any> = new EventEmitter<any>();
  @Input() entityActiveTabName :string;

  isViewEntityDetails = false;
  entityList :any;
  constructor(private _router: Router, private _entityManagementService: EntityManagementService) { }

  ngOnInit() {

console.log(this.entityActiveTabName);

    // this.viewListOfEntity();
  }

  viewDetails(entityListData) {
     this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: entityListData.coiEntityId } })
    // this.viewEntityDetails.emit(value);
  }

  viewListOfEntity() {
    this._entityManagementService.getAllSystemEntityList().subscribe((res:any) => {
      this.entityList = res.coiEntityList;
    });
  }
  ngOnChanges(){
    this.viewListOfEntity();
    console.log('onChange',this.entityActiveTabName);
  }
}
