import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { hideModal, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { SfiService } from '../../disclosure/sfi/sfi.service';

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

  constructor(private _router:Router, private _sfiService: SfiService) { }

  ngOnInit() {
    openModal('addRelationshipModal');
  }

  closeModal() {
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);

  }

  addRelation(){
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104' }});
  }

  continueWithoutRelation() {
    this._sfiService.$addSfi.next(true);
    hideModal('addRelationshipModal');
    this._sfiService.isShowSfiNavBar = false;
  }

}
