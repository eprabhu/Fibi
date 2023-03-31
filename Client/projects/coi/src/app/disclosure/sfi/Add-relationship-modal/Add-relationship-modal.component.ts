import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import bootstrap from "../../../../assets/js/bootstrap.bundle.min.js";

declare var $: any;
@Component({
  selector: 'app-Add-relationship-modal',
  templateUrl: './Add-relationship-modal.component.html',
  styleUrls: ['./Add-relationship-modal.component.scss']
})
export class AddRelationshipModalComponent implements OnInit {

  @Output() relationshipResult: EventEmitter<any> = new EventEmitter<any>();

  isAddRelationshipModal: any;

  constructor(private _router:Router) { }

  ngOnInit() {
    let myModal = new bootstrap.Modal(document.getElementById('addRelationshipModal'));
    myModal.show();

  }

  closeModal() {
    this.relationshipResult.emit(false);
  }

  withoutRelation(){

    let myModal = new bootstrap.Modal(document.getElementById('addRelationshipModal'));
    myModal.hide();
    this._router.navigate(['/coi/entity-details']);
  }
}
