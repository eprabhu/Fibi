import { Component, EventEmitter, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss']
})
export class EntityListComponent implements OnInit {

  @Output() viewEntityDetails: EventEmitter<any> = new EventEmitter<any>();

  isViewEntityDetails = false;
  entityList = [];
  constructor() { }

  ngOnInit() {
    this.viewListOfEntity();
  }

  viewDetails(value) {
    this.viewEntityDetails.emit(value);
  }

  viewListOfEntity() {
    this.entityList = [
      { 'name': 'Alexion Pharmaceuticals', 'country': 'US', 'entityType': 'For Profit', 'riskLevel': 'High', 'status': 'Active' },
      { 'name': 'Bausch health companies', 'country': 'Canada', 'entityType': 'Non Profit', 'riskLevel': 'Medium', 'status': 'Inactive' },
      { 'name': 'Daichi Sankyo Co., Ltd.', 'country': 'Japan', 'entityType': 'Non Profit', 'riskLevel': 'Low', 'status': 'Inactive' }
    ];


  }
}
