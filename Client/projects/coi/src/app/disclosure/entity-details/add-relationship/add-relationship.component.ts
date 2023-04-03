import { Component, OnInit } from '@angular/core';
import { hideModal } from '../../../../../../../projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-add-relationship',
  templateUrl: './add-relationship.component.html',
  styleUrls: ['./add-relationship.component.scss']
})
export class AddRelationshipComponent implements OnInit {

  constructor() { }
  isShowRelationshipModal
  ngOnInit() {
  }

  addRelationShip() {
    this.isShowRelationshipModal = true;
  }

  addEntityToggle(event) {
    hideModal(event)
  }
  hideRelationshipModal(event) {
    this.isShowRelationshipModal = event
  }
}
