import { Component, EventEmitter, OnInit, Output } from '@angular/core';

declare var $: any;
@Component({
  selector: 'app-Add-relationship-modal',
  templateUrl: './Add-relationship-modal.component.html',
  styleUrls: ['./Add-relationship-modal.component.scss']
})
export class AddRelationshipModalComponent implements OnInit {

  @Output() relationshipResult: EventEmitter<any> = new EventEmitter<any>();

  isAddRelationshipModal: any;
  
  constructor() { }

  ngOnInit() {
    const modal = document.getElementById('btn-add-relationships');
    modal.click();
  }

  closeModal() {
    this.relationshipResult.emit(false);
  }
}
