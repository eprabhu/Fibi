import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormListComponent } from './form-list.component';
import { RouterModule } from '@angular/router';

@NgModule({
	imports: [
		CommonModule,
		RouterModule.forChild([
			{ path: '', component: FormListComponent }
		])],
	declarations: [FormListComponent]
})

export class FormListModule { }


