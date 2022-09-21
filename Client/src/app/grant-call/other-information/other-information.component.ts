import { Component, OnInit } from '@angular/core';
import { GrantCommonDataService } from '../services/grant-common-data.service';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-other-information',
  template: `<div id ="grant-other-information-section"><app-custom-element *ngIf="grantId" [moduleItemKey]="grantId" [moduleCode]='15'
                [viewMode]="_commonData.isViewMode ? 'view' : 'edit'" (dataChangeEvent)="dataChangeEvent($event)"></app-custom-element> </div>`,

})
export class OtherInformationComponent implements OnInit {

  grantId: any;
  viewMode: string;

  constructor(public _commonData: GrantCommonDataService, private _activeRoute: ActivatedRoute,) { }

  ngOnInit() {
    this.grantId = this._activeRoute.snapshot.queryParamMap.get('grantId');;
  }

  dataChangeEvent(event) {
    this._commonData.isGrantCallDataChange = event;
  }

}
