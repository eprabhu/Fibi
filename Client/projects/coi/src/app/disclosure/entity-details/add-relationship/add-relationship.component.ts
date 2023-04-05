import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { hideModal } from '../../../../../../../projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-add-relationship',
  templateUrl: './add-relationship.component.html',
  styleUrls: ['./add-relationship.component.scss']
})
export class AddRelationshipComponent implements OnInit {
@Output() addRelationShip: EventEmitter<any> = new EventEmitter<any>();
  constructor() { }
  isShowRelationshipModal
  ngOnInit() {
  }

  addRelationShips() {
    this.addRelationShip.emit('addRelationshipModal');
  }

  addEntityToggle(event) {

  }
  hideRelationshipModal(event) {
    this.isShowRelationshipModal = event
  }
}
