import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { openModal} from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-add-relationship-modal',
  templateUrl: './add-relationship-modal.component.html',
  styleUrls: ['./add-relationship-modal.component.scss']
})
export class AddRelationshipModalComponent implements OnInit {

  @Output() relationshipResult: EventEmitter<any> = new EventEmitter<any>();
  @Output() hideModal = new EventEmitter<boolean>()
  @Input() isWithOutRelationship = false;

  isAddRelationshipModal: any;

  constructor(private _router:Router) { }

  ngOnInit() {
    openModal('addRelationshipModal');
  }

  closeModal() {
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);

  }

  withoutRelation(){
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);
    // this._router.navigate(['/coi/entity-details']);
  }
}
