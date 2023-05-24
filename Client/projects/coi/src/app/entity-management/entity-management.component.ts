import { Component, OnInit } from '@angular/core';
import { EntityManagementService } from './entity-management.service';
@Component({
  selector: 'app-entity-management',
  templateUrl: './entity-management.component.html',
  styleUrls: ['./entity-management.component.scss'],
  providers: [EntityManagementService]
})
export class EntityManagementComponent implements OnInit {

  constructor() {
  }

  ngOnInit() {}

}


