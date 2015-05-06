/*  Author: Joe Barrett
 *  Description: Class to control inputs on initial GUI
 *  Date Last Modified: 09/10/2014
 */
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Coursework
{
    public partial class GUI : Window
    {
        Staff staff = new Staff();
        Demonstrator demonstrator = new Demonstrator();

        public GUI()
        {
            InitializeComponent();
        }

        //  Method to clear all textboxes when clear button pressed
        private void btnClear_Click(object sender, RoutedEventArgs e)
        {
            clearAllTxtBox();
        }

        //  Method to show Payslip window
        private void btnCalculatePay_Click(object sender, RoutedEventArgs e)
        {
            if (rdoStaff.IsChecked == true)
            {
                Payslip payslip = new Payslip(staff);
                payslip.ShowDialog();
            }
            if (rdoDemon.IsChecked == true)
            {
                Payslip payslip = new Payslip(demonstrator);
                payslip.ShowDialog();
            }
        }

        //  Method to set all Staff or Demonstrator properties from textboxes when button is pressed
        private void btnSet_Click(object sender, RoutedEventArgs e)
        {
            if (rdoStaff.IsChecked == true)
            {
                setStaff();
            }
            if (rdoDemon.IsChecked == true)
            {
                setDemonstrator();
            }

        }

        //  Method to return all properties from staff or demonstrator classes to textboxes
        private void btnGet_Click(object sender, RoutedEventArgs e)
        {
            if (rdoStaff.IsChecked == true)
            {
                getStaff();
            }
            if (rdoDemon.IsChecked == true)
            {
                getDemonstrator();
            }
        }

        //  Method to set all staff atributes from appropriate textboxes
        private void setStaff()
        {
            try
            {
                staff.FirstName = txtFirstName.Text;
                staff.StaffId = int.Parse(txtStaffId.Text);
                staff.SecondName = txtLastName.Text;
                staff.DateOfBirth = DateTime.Parse(txtDob.Text);
                staff.Department = txtDepartment.Text;
                staff.HourlyPayRate = float.Parse(removePound(txtPayRate.Text));
                staff.HoursWorked = float.Parse(txtHoursWorked.Text);
                staff.GrossPay = staff.HourlyPayRate * staff.HoursWorked;
                clearAllTxtBox();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message);
            }
        }

        //  Method to set all demonstrator atributes from appropriate textboxes
        private void setDemonstrator()
        {
            try
            {
                demonstrator.FirstName = txtFirstName.Text;
                demonstrator.StaffId = int.Parse(txtStaffId.Text);
                demonstrator.SecondName = txtLastName.Text;
                demonstrator.DateOfBirth = DateTime.Parse(txtDob.Text);
                demonstrator.Department = txtDepartment.Text;
                demonstrator.HourlyPayRate = float.Parse(removePound(txtPayRate.Text));
                demonstrator.HoursWorked = float.Parse(txtHoursWorked.Text);
                demonstrator.GrossPay = demonstrator.HourlyPayRate * demonstrator.HoursWorked;
                demonstrator.Course = txtCourse.Text;
                demonstrator.Supervisor = txtSupervisor.Text;
                clearAllTxtBox();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message);
            }
        }

        //  Method to populate all appropriate textboxes from attributes in Staff class
        private void getStaff()
        {
            clearAllTxtBox();
            txtFirstName.Text = staff.FirstName;
            txtLastName.Text = staff.SecondName;
            txtDob.Text = staff.DateOfBirth.ToShortDateString();
            txtDepartment.Text = staff.Department;
            txtStaffId.Text = staff.StaffId.ToString();
            txtPayRate.Text = "£" + (staff.HourlyPayRate / 100).ToString("0.00");
            txtHoursWorked.Text = staff.HoursWorked.ToString();
        }

        //  Method to populate all appropriate textboxes from attributes in Demonstrator class
        private void getDemonstrator()
        {
            clearAllTxtBox();
            txtFirstName.Text = demonstrator.FirstName;
            txtLastName.Text = demonstrator.SecondName;
            txtDob.Text = demonstrator.DateOfBirth.ToShortDateString();
            txtDepartment.Text = demonstrator.Department;
            txtStaffId.Text = demonstrator.StaffId.ToString();
            txtPayRate.Text = "£" + (demonstrator.HourlyPayRate / 100).ToString("0.00");
            txtHoursWorked.Text = demonstrator.HoursWorked.ToString();
            txtCourse.Text = demonstrator.Course;
            txtSupervisor.Text = demonstrator.Supervisor;
        }

        //  Method to clear all textboxes on screen
        private void clearAllTxtBox()
        {
            txtFirstName.Clear();
            txtLastName.Clear();
            txtDob.Clear();
            txtDepartment.Clear();
            txtStaffId.Clear();
            txtPayRate.Clear();
            txtHoursWorked.Clear();
            txtCourse.Clear();
            txtSupervisor.Clear();
        }

        //  Method to remove automatic pound sign when setting properties 
        private static string removePound(string input)
        {
            return input.Replace("£", "");
        }
    }
}
