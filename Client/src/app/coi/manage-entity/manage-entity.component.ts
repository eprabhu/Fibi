import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { DataStoreService } from '../services/data-store.service';

@Component({
  selector: 'app-manage-entity',
  templateUrl: './manage-entity.component.html',
  styleUrls: ['./manage-entity.component.css']
})
export class ManageEntityComponent implements OnInit {

  isHeaderExpanded = true;
  status = 'A';
  entityId: any;
  entityDetails: any = {};

  constructor(private _dataStore: DataStoreService,
    private _route: ActivatedRoute) { }

  ngOnInit() {
    this.entityId = this._route.snapshot.queryParamMap.get('entityId');
    this.entityDetails = this._dataStore.entityDetails.find(ele => ele.entityId == this.entityId);
  }

}
