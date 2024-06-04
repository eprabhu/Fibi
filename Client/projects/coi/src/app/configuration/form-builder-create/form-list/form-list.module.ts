import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormListComponent } from './form-list.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../../shared/shared.module';

@NgModule({
	imports: [
		CommonModule,
		SharedModule,
		RouterModule.forChild([
			{ path: '', component: FormListComponent }
		])],
	declarations: [FormListComponent]
})

export class FormListModule { }


