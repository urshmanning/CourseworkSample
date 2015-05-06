package appointmentbooking.manager;

import javax.swing.*;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.*;

public class HomeScreen  extends JPanel{

	private JFrame frame;
	private JPanel buttonPanel; 
	
	public void createHomeScreen(){
		frame = new JFrame();
		buttonPanel = new JPanel();
		
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		buttonPanel.setLayout(new FlowLayout());
		
		JLabel  heading = new JLabel("Hospital Booking ", JLabel.CENTER);
		JLabel  instruction = new JLabel("Pick your role:", JLabel.LEFT);
		  
		JButton patient = new JButton("Patient");
		JButton specialist = new JButton("Specialist");
		JButton administrator = new JButton("Administrator");
		
		buttonPanel.add(instruction);
		buttonPanel.add(patient);
		buttonPanel.add(specialist);
		buttonPanel.add(administrator);
		
		frame.setLayout(new GridLayout(2, 1));
		frame.setSize(500,500);
		frame.setVisible(true);
		
		frame.add(heading);
		frame.add(buttonPanel);
	}
	
}
